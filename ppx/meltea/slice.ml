open Ppxlib
open Ast_builder.Default

(** What a local of a view reads, as the guards of its templates see it. *)
type local =
  | Exact of int  (** These fields' bits; its guard changes exactly when one of them does. *)
  | Coarse of int  (** These fields' bits; its guard can change more often, as when it is made on every draw. *)
  | Opaque  (** Anything: only its guard can tell. *)
  | Model  (** The view's parameter: [model.f] reads [f]. *)

(** What a part of the view's spine is to its page. *)
type need = Needed | Unneeded

(** A template site, by where its JSX is in the source. *)
type site = { start : int; stop : int; file : string }

type t = {
  needs : need list;
  sites : (site * (string * local) list) list;
  closers : int list option;  (** The bits each message of the module's [update] writes, when it can be read. *)
  mutable gated : int;  (** How many holes the compiled view narrowed. *)
}

(** Whether a value reads its fields more finely than a guard on its locals does. *)
type narrowing = Narrows | Matches

(** What a value reads: field bits, the locals only their guards can follow, and how finely. *)
type reading = { bits : int; unmasked : string list; narrowing : narrowing }

exception Unread

let reading env ~locals expression =
  let found = ref { bits = 0; unmasked = []; narrowing = Matches } in
  let add bits narrowing =
    found :=
      {
        !found with
        bits = !found.bits lor bits;
        narrowing = (match (narrowing, !found.narrowing) with Narrows, _ | _, Narrows -> Narrows | Matches, Matches -> Matches);
      }
  in
  let unmask name = found := { !found with unmasked = name :: !found.unmasked } in
  let rec read scope expression =
    let cases scope cases =
      List.iter
        (fun case ->
          let scope = Melhole_compiler.Free.bound case.pc_lhs @ scope in
          Option.iter (read scope) case.pc_guard;
          read scope case.pc_rhs)
        cases
    in
    match expression.pexp_desc with
    | Pexp_ident { txt = Lident name; _ } when List.mem name scope -> ()
    | Pexp_ident { txt = Lident name; _ } -> (
        match List.assoc_opt name env with
        | Some (Exact bits) -> add bits Matches
        | Some (Coarse bits) -> add bits Narrows
        | Some (Opaque | Model) -> unmask name
        | None -> ( match List.mem name locals with true -> unmask name | false -> ()))
    | Pexp_ident { txt = Ldot (Ldot (Lident "Js", ("Date" | "Math")), _) | Ldot (Lident ("Random" | "Sys" | "Unix"), _); _ } ->
        raise Unread
    | Pexp_ident _ | Pexp_constant _ -> ()
    | Pexp_field (_, { txt = Lident ("contents" | "current") | Ldot (_, ("contents" | "current")); _ }) -> raise Unread
    | Pexp_field (({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } as target), { txt; _ }) -> (
        match (List.mem name scope, List.assoc_opt name env) with
        | false, Some Model -> add (Fields.mask (Named [ Longident.last_exn txt ])) Narrows
        | true, _ | false, (Some (Exact _ | Coarse _ | Opaque) | None) -> read scope target)
    | Pexp_field (target, _) -> read scope target
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident ("!" | ":=" | "incr" | "decr"); _ }; _ }, _) -> raise Unread
    | Pexp_apply (callee, arguments) ->
        read scope callee;
        List.iter (fun (_, argument) -> read scope argument) arguments
    | Pexp_setfield ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _, value)
      when List.mem name scope && String.starts_with ~prefix:"__melhole" name ->
        read scope value
    | Pexp_function (parameters, _, body) -> (
        let scope =
          List.fold_left
            (fun scope parameter ->
              match parameter.pparam_desc with
              | Pparam_val (_, default, pattern) ->
                  Option.iter (read scope) default;
                  Melhole_compiler.Free.bound pattern @ scope
              | Pparam_newtype _ -> scope)
            scope parameters
        in
        match body with Pfunction_body body -> read scope body | Pfunction_cases (branches, _, _) -> cases scope branches)
    | Pexp_let (flag, bindings, body) ->
        let names = List.concat_map (fun binding -> Melhole_compiler.Free.bound binding.pvb_pat) bindings in
        List.iter
          (fun binding -> read (match flag with Recursive -> names @ scope | Nonrecursive -> scope) binding.pvb_expr)
          bindings;
        read (names @ scope) body
    | Pexp_match (target, branches) | Pexp_try (target, branches) ->
        read scope target;
        cases scope branches
    | Pexp_tuple items | Pexp_array items -> List.iter (read scope) items
    | Pexp_construct (_, argument) | Pexp_variant (_, argument) -> Option.iter (read scope) argument
    | Pexp_record (fields, base) ->
        List.iter (fun (_, value) -> read scope value) fields;
        Option.iter (read scope) base
    | Pexp_ifthenelse (test, yes, no) ->
        read scope test;
        read scope yes;
        Option.iter (read scope) no
    | Pexp_sequence (first, second) ->
        read scope first;
        read scope second
    | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) | Pexp_newtype (_, inner) -> read scope inner
    | _ -> raise Unread
  in
  match read [] expression with () -> Some !found | exception Unread -> None

let site_of (loc : location) = { start = loc.loc_start.pos_cnum; stop = loc.loc_end.pos_cnum; file = loc.loc_start.pos_fname }

let plan ~closers binding =
  let rec parameter pattern =
    match pattern.ppat_desc with
    | Ppat_var { txt; _ } -> Some [ (txt, Model) ]
    | Ppat_record (fields, _) ->
        Some
          (List.concat_map
             (fun ({ txt; _ }, inner) ->
               List.map (fun name -> (name, Exact (Fields.mask (Named [ Longident.last_exn txt ])))) (Melhole_compiler.Free.bound inner))
             fields)
    | Ppat_alias (inner, { txt; _ }) -> Option.map (fun found -> (txt, Model) :: found) (parameter inner)
    | Ppat_constraint (inner, _) -> parameter inner
    | _ -> None
  in
  match binding.pvb_expr.pexp_desc with
  | Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], _, Pfunction_body body) ->
      Option.bind (parameter pattern) (fun parameters ->
          let identifiers expression =
            (object
               inherit [string list] Ast_traverse.fold as super

               method! expression expression found =
                 match expression.pexp_desc with
                 | Pexp_ident { txt = Lident name; _ } -> name :: found
                 | _ -> super#expression expression found
            end)
              #expression expression []
          in
          let rec spine body =
            match body.pexp_desc with
            | Pexp_let (flag, bindings, rest) ->
                let items, tail = spine rest in
                (`Binds (flag, bindings) :: items, tail)
            | Pexp_sequence (first, rest) ->
                let items, tail = spine rest in
                (`Runs first :: items, tail)
            | _ -> ([], body)
          in
          let items, tail = spine body in
          let names item =
            match item with
            | `Binds (_, bindings) -> List.concat_map (fun binding -> Melhole_compiler.Free.bound binding.pvb_pat) bindings
            | `Runs _ -> []
          in
          let read item =
            match item with
            | `Binds (_, bindings) -> List.concat_map (fun binding -> identifiers binding.pvb_expr) bindings
            | `Runs first -> identifiers first
          in
          let needs =
            snd
              (List.fold_right
                 (fun item (wanted, needs) ->
                   match List.exists (fun name -> List.mem name wanted) (names item) with
                   | true -> (read item @ wanted, Needed :: needs)
                   | false -> (wanted, Unneeded :: needs))
                 items (identifiers tail, []))
          in
          let models = List.filter_map (fun (name, local) -> match local with Model -> Some name | Exact _ | Coarse _ | Opaque -> None) parameters in
          let rebinding = List.find_opt (fun item -> List.exists (fun name -> List.mem name models) (names item)) items in
          let leaking =
            snd
              (List.fold_left
                 (fun (made, leaking) (item, need) ->
                   match need with
                   | Needed -> (names item @ made, leaking)
                   | Unneeded -> (made, List.filter (fun name -> List.mem name made) (read item) @ leaking))
                 ([], []) (List.combine items needs))
          in
          match (rebinding, leaking) with
          | Some _, _ | _, _ :: _ -> None
          | None, [] ->
              let sites = ref [] in
              let rec visit env expression =
                (match Melhole_compiler.Element.without_jsx expression.pexp_attributes with
                | Some _ -> sites := (site_of expression.pexp_loc, env) :: !sites
                | None -> ());
                let opaque names env = List.map (fun name -> (name, Opaque)) names @ env in
                match expression.pexp_desc with
                | Pexp_let (flag, bindings, body) ->
                    let names = List.concat_map (fun binding -> Melhole_compiler.Free.bound binding.pvb_pat) bindings in
                    List.iter
                      (fun binding -> visit (match flag with Recursive -> opaque names env | Nonrecursive -> env) binding.pvb_expr)
                      bindings;
                    visit (opaque names env) body
                | Pexp_match (target, cases) | Pexp_try (target, cases) ->
                    visit env target;
                    List.iter
                      (fun case ->
                        let env = opaque (Melhole_compiler.Free.bound case.pc_lhs) env in
                        Option.iter (visit env) case.pc_guard;
                        visit env case.pc_rhs)
                      cases
                | Pexp_apply (callee, arguments) ->
                    visit env callee;
                    List.iter (fun (_, argument) -> visit env argument) arguments
                | Pexp_tuple items | Pexp_array items -> List.iter (visit env) items
                | Pexp_construct (_, argument) | Pexp_variant (_, argument) -> Option.iter (visit env) argument
                | Pexp_record (fields, base) ->
                    List.iter (fun (_, value) -> visit env value) fields;
                    Option.iter (visit env) base
                | Pexp_field (target, _) -> visit env target
                | Pexp_ifthenelse (test, yes, no) ->
                    visit env test;
                    visit env yes;
                    Option.iter (visit env) no
                | Pexp_sequence (first, second) ->
                    visit env first;
                    visit env second
                | Pexp_constraint (inner, _) | Pexp_coerce (inner, _, _) -> visit env inner
                | _ -> ()
              in
              let kind env expression =
                let exact input =
                  match input.pexp_desc with
                  | Pexp_construct ({ txt = Ldot (Ldot (Lident "Melhole", "View"), "Input"); _ }, Some inner) -> (
                      match inner.pexp_desc with
                      | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _) -> (
                          match List.assoc_opt name env with Some Model -> true | Some (Exact _ | Coarse _ | Opaque) | None -> false)
                      | Pexp_ident { txt = Lident name; _ } -> (
                          match List.assoc_opt name env with Some (Exact _) -> true | Some (Coarse _ | Opaque | Model) | None -> false)
                      | _ -> false)
                  | _ -> false
                in
                match (expression.pexp_desc, reading env ~locals:[] expression) with
                | _, None | _, Some { unmasked = _ :: _; _ } -> Opaque
                | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _), Some { bits; _ }
                  when List.mem name models ->
                    Exact bits
                | Pexp_ident { txt = Lident name; _ }, Some _ -> (
                    match List.assoc_opt name env with Some (Exact _ | Coarse _ as same) -> same | Some (Opaque | Model) | None -> Opaque)
                | Pexp_function _, Some { bits; narrowing = Matches; _ } -> Exact bits
                | ( Pexp_apply
                      ( { pexp_desc = Pexp_ident { txt = Ldot (Ldot (Lident "Melhole", "View"), "remember"); _ }; _ },
                        [ _; (_, { pexp_desc = Pexp_array inputs; _ }); _ ] ),
                    Some { bits; _ } )
                  when List.for_all exact inputs ->
                    Exact bits
                | _, Some { bits; _ } -> Coarse bits
              in
              let env =
                List.fold_left
                  (fun env (item, need) ->
                    match (item, need) with
                    | `Runs _, _ | `Binds _, Unneeded -> env
                    | `Binds (Recursive, bindings), Needed ->
                        let names = names item in
                        List.iter (fun binding -> visit (List.map (fun name -> (name, Opaque)) names @ env) binding.pvb_expr) bindings;
                        List.map (fun name -> (name, Opaque)) names @ env
                    | `Binds (Nonrecursive, bindings), Needed ->
                        List.iter (fun binding -> visit env binding.pvb_expr) bindings;
                        List.concat_map
                          (fun binding ->
                            match binding.pvb_pat.ppat_desc with
                            | Ppat_var { txt; _ } -> [ (txt, kind env binding.pvb_expr) ]
                            | _ ->
                                let made = match kind env binding.pvb_expr with Exact bits | Coarse bits -> Coarse bits | Opaque | Model -> Opaque in
                                List.map (fun name -> (name, made)) (Melhole_compiler.Free.bound binding.pvb_pat))
                          bindings
                        @ env)
                  parameters (List.combine items needs)
              in
              visit env tail;
              Some { needs; sites = !sites; closers; gated = 0 })
  | _ -> None

let gates t ~locals loc =
  let at = site_of loc in
  match
    List.filter (fun ({ start; stop; file }, _) -> start = at.start && stop = at.stop && String.equal file at.file) t.sites
  with
  | [ (_, env) ] ->
      Melhole_compiler.Element.Gates
        {
          opened = (fun slot -> [%expr Program.opened [%e slot] __melhole_changed]);
          gate =
            (fun value guarded ->
              match (reading env ~locals value, guarded) with
              | None, _ | Some { narrowing = Matches; _ }, Guarded -> Always
              | Some { bits; unmasked; _ }, (Guarded | Unguarded) -> (
                  match
                    Option.fold ~none:(Some 0)
                      ~some:(List.find_opt (fun written -> written <> 0 && written land bits = 0))
                      t.closers
                  with
                  | None -> Always
                  | Some _ ->
                      t.gated <- t.gated + 1;
                      Gated { mask = bits lor (1 lsl 30); unmasked = List.sort_uniq String.compare unmasked }));
        }
  | _ -> Ungated

let split t binding =
  let loc = binding.pvb_expr.pexp_loc in
  match binding.pvb_expr.pexp_desc with
  | Pexp_function ([ ({ pparam_desc = Pparam_val (Nolabel, None, pattern); _ } as parameter) ], constraint_, Pfunction_body body)
    -> (
      let rec spine needs body =
        match (needs, body.pexp_desc) with
        | need :: rest, Pexp_let (flag, bindings, next) ->
            Option.map
              (fun (items, tail) -> ((need, fun inner -> { body with pexp_desc = Pexp_let (flag, bindings, inner) }) :: items, tail))
              (spine rest next)
        | need :: rest, Pexp_sequence (first, next) ->
            Option.map
              (fun (items, tail) -> ((need, fun inner -> { body with pexp_desc = Pexp_sequence (first, inner) }) :: items, tail))
              (spine rest next)
        | [], _ -> Some ([], body)
        | _ :: _, _ -> None
      in
      match (spine t.needs body, t.gated, List.find_opt (fun need -> match need with Unneeded -> true | Needed -> false) t.needs) with
      | Some _, 0, None -> (binding, None)
      | None, _, _ ->
          ( {
              binding with
              pvb_expr =
                {
                  binding.pvb_expr with
                  pexp_desc =
                    Pexp_function ([ parameter ], constraint_, Pfunction_body [%expr let __melhole_changed = -1 in [%e body]]);
                };
            },
            None )
      | Some (items, tail), _, _ ->
          let kept wanted inner =
            List.fold_right
              (fun (need, rebuild) inner ->
                match (need, wanted) with
                | Needed, Needed | Unneeded, Unneeded -> rebuild inner
                | Needed, Unneeded | Unneeded, Needed -> inner)
              items inner
          in
          let slice =
            Melhole_compiler.Hoist.hoist ~loc ~unused:Melhole_compiler.Hoist.Kept "slice"
              [%expr
                (fun __melhole_changed -> [%e pexp_function ~loc [ parameter ] constraint_ (Pfunction_body (kept Needed tail))])
                [@ocaml.warning "-26-27"]]
          in
          let model, given =
            match (pattern.ppat_desc, List.exists (fun (need, _) -> match need with Unneeded -> true | Needed -> false) items) with
            | Ppat_var { txt; _ }, _ -> (txt, pattern)
            | _, false -> ("__melhole_model", pvar ~loc "__melhole_model")
            | _, true -> ("__melhole_model", ppat_alias ~loc pattern { txt = "__melhole_model"; loc })
          in
          ( {
              binding with
              pvb_expr =
                {
                  binding.pvb_expr with
                  pexp_desc =
                    Pexp_function
                      ( [ { parameter with pparam_desc = Pparam_val (Nolabel, None, given) } ],
                        None,
                        Pfunction_body (kept Unneeded [%expr [%e slice] (-1) [%e evar ~loc model]]) );
                };
              pvb_attributes =
                attribute ~loc ~name:{ txt = "ocaml.warning"; loc } ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-26-27") [] ])
                :: binding.pvb_attributes;
            },
            Some slice ))
  | _ -> (binding, None)
