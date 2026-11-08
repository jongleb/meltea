open Ppxlib
open Ast_builder.Default

type fields = Every | Named of string list

let union first second =
  match (first, second) with
  | Every, _ | _, Every -> Every
  | Named first, Named second -> Named (List.sort_uniq String.compare (first @ second))

let mask fields =
  match fields with
  | Every -> -1
  | Named names -> List.fold_left (fun mask name -> mask lor (1 lsl (Hashtbl.hash name mod 30))) 0 names

type change = Kept | Literals of string * string list | Set
type outcome = Model of (string * change) list | Unknown
type coverage = Total | Partial
type branch = { pattern : pattern; coverage : coverage; outcome : outcome }

let join first second =
  let changed first second =
    match (first, second) with
    | Set, _ | _, Set -> Set
    | Kept, other | other, Kept -> other
    | Literals (one, more), Literals (other, rest) -> (
        match List.sort_uniq String.compare ((one :: more) @ (other :: rest)) with
        | least :: others -> Literals (least, others)
        | [] -> Set)
  in
  match (first, second) with
  | Unknown, _ | _, Unknown -> Unknown
  | Model first, Model second ->
      Model
        (List.filter_map
           (fun name ->
             let change fields = Option.value ~default:Kept (List.assoc_opt name fields) in
             match changed (change first) (change second) with Kept -> None | joined -> Some (name, joined))
           (List.sort_uniq String.compare (List.map fst first @ List.map fst second)))

let written outcome =
  match outcome with
  | Unknown -> Every
  | Model fields ->
      Named (List.sort_uniq String.compare (List.filter_map (fun (name, change) -> match change with Kept -> None | Literals _ | Set -> Some name) fields))

(** What a name local to [update] stands for. *)
type local =
  | Value of outcome  (** A value made from the model. *)
  | Helper of pattern list * expression * (string * local) list  (** A function, with the names it closes over. *)

let shadowed names env = List.filter (fun (name, _) -> not (List.mem name names)) env

let rec outcome ~model env expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> (
      match List.assoc_opt name env with
      | Some (Value found) -> found
      | Some (Helper _) -> Unknown
      | None -> ( match String.equal name model with true -> Model [] | false -> Unknown))
  | Pexp_record (fields, base) -> (
      match Option.fold ~none:(Model []) ~some:(outcome ~model env) base with
      | Unknown -> Unknown
      | Model kept ->
          let change field written =
            match written.pexp_desc with
            | Pexp_field (owner, { txt; _ }) when String.equal (Longident.last_exn txt) field -> (
                match outcome ~model env owner with
                | Model from -> Option.value ~default:Kept (List.assoc_opt field from)
                | Unknown -> Set)
            | Pexp_construct ({ txt; _ }, None) -> Literals (Longident.name txt, [])
            | Pexp_variant (label, None) -> Literals ("`" ^ label, [])
            | Pexp_constant constant -> Literals (Pprintast.string_of_expression (pexp_constant ~loc:written.pexp_loc constant), [])
            | _ -> Set
          in
          let named = List.map (fun ({ txt; _ }, written) -> (Longident.last_exn txt, written)) fields in
          Model
            (List.filter_map
               (fun (name, change) -> match change with Kept -> None | Literals _ | Set -> Some (name, change))
               (List.filter (fun (name, _) -> not (List.mem_assoc name named)) kept
               @ List.map (fun (name, written) -> (name, change name written)) named)))
  | Pexp_tuple (first :: _) -> outcome ~model env first
  | Pexp_match (_, cases) | Pexp_try (_, cases) ->
      List.fold_left
        (fun found case ->
          let bound = Melhole_compiler.Free.bound case.pc_lhs in
          match List.mem model bound with
          | true -> Unknown
          | false -> join found (outcome ~model (shadowed bound env) case.pc_rhs))
        (Model []) cases
  | Pexp_ifthenelse (_, yes, Some no) -> join (outcome ~model env yes) (outcome ~model env no)
  | Pexp_let (flag, bindings, body) -> (
      let names = List.concat_map (fun binding -> Melhole_compiler.Free.bound binding.pvb_pat) bindings in
      match (List.mem model names, flag) with
      | true, _ -> Unknown
      | false, Recursive -> outcome ~model (shadowed names env) body
      | false, Nonrecursive -> outcome ~model (bind ~model env bindings) body)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, arguments) -> (
      match List.assoc_opt name env with
      | Some (Helper (parameters, body, closed))
        when List.length parameters = List.length arguments
             && List.for_all (fun (label, _) -> match label with Nolabel -> true | Labelled _ | Optional _ -> false) arguments ->
          let bound = List.concat_map Melhole_compiler.Free.bound parameters in
          if List.mem model bound then Unknown else outcome ~model (shadowed bound closed) body
      | Some (Helper _ | Value _) | None -> Unknown)
  | Pexp_sequence (_, rest) | Pexp_constraint (rest, _) -> outcome ~model env rest
  | _ -> Unknown

and bind ~model env bindings =
  List.concat_map
    (fun binding ->
      let plain parameter =
        match parameter.pparam_desc with
        | Pparam_val (Nolabel, None, pattern) -> Some pattern
        | Pparam_val _ | Pparam_newtype _ -> None
      in
      match (binding.pvb_pat.ppat_desc, binding.pvb_expr.pexp_desc) with
      | Ppat_var { txt; _ }, Pexp_function (parameters, None, Pfunction_body body)
        when List.for_all (fun parameter -> Option.is_some (plain parameter)) parameters ->
          [ (txt, Helper (List.filter_map plain parameters, body, env)) ]
      | Ppat_var { txt; _ }, _ -> [ (txt, Value (outcome ~model env binding.pvb_expr)) ]
      | _, _ -> List.map (fun name -> (name, Value Unknown)) (Melhole_compiler.Free.bound binding.pvb_pat))
    bindings
  @ shadowed (List.concat_map (fun binding -> Melhole_compiler.Free.bound binding.pvb_pat) bindings) env

let lattice branches = List.fold_left (fun found branch -> join found branch.outcome) (Model []) branches

let branches expression =
  let rec project pattern =
    let rec irrefutable pattern =
      match pattern.ppat_desc with
      | Ppat_any | Ppat_var _ -> true
      | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> irrefutable inner
      | Ppat_tuple items -> List.for_all irrefutable items
      | _ -> false
    in
    match pattern.ppat_desc with
    | Ppat_tuple (first :: rest) -> (first, match List.for_all irrefutable rest with true -> Total | false -> Partial)
    | Ppat_or (left, right) ->
        let left, left_coverage = project left and right, right_coverage = project right in
        ( { pattern with ppat_desc = Ppat_or (left, right) },
          match (left_coverage, right_coverage) with Total, Total -> Total | Partial, _ | _, Partial -> Partial )
    | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> project inner
    | Ppat_any | Ppat_var _ -> (ppat_any ~loc:pattern.ppat_loc, Total)
    | _ -> (ppat_any ~loc:pattern.ppat_loc, Partial)
  in
  let cases ~model env projected cases =
    List.map
      (fun case ->
        let bound = Melhole_compiler.Free.bound case.pc_lhs in
        let pattern, coverage = projected case.pc_lhs in
        {
          pattern;
          coverage = Option.fold ~none:coverage ~some:(fun _ -> Partial) case.pc_guard;
          outcome = (match List.mem model bound with true -> Unknown | false -> outcome ~model (shadowed bound env) case.pc_rhs);
        })
      cases
  in
  let rec dispatch ~message ~model env body =
    match body.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, rest)
      when not
             (List.exists
                (fun binding ->
                  List.exists
                    (fun name -> String.equal name message || String.equal name model)
                    (Melhole_compiler.Free.bound binding.pvb_pat))
                bindings) ->
        dispatch ~message ~model (bind ~model env bindings) rest
    | Pexp_match ({ pexp_desc = Pexp_ident { txt = Lident scrutinee; _ }; _ }, branches) when String.equal message scrutinee ->
        Some (cases ~model env (fun pattern -> (pattern, Total)) branches)
    | Pexp_match
        ({ pexp_desc = Pexp_tuple ({ pexp_desc = Pexp_ident { txt = Lident scrutinee; _ }; _ } :: _ :: _); _ }, branches)
      when String.equal message scrutinee ->
        Some (cases ~model env project branches)
    | _ -> None
  in
  match expression.pexp_desc with
  | Pexp_function
      ( [
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = message; _ }; _ }); _ };
          { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = model; _ }; _ }); _ };
        ],
        None,
        Pfunction_body body ) ->
      dispatch ~message ~model [] body
  | _ -> None

let update ~loc expression =
  Option.map
    (fun branches ->
      let rec heads pattern =
        match pattern.ppat_desc with
        | Ppat_construct ({ txt; _ }, _) -> Some [ Longident.last_exn txt ]
        | Ppat_variant (label, _) -> Some [ "`" ^ label ]
        | Ppat_or (first, second) -> Option.bind (heads first) (fun first -> Option.map (fun second -> first @ second) (heads second))
        | Ppat_alias (inner, _) | Ppat_constraint (inner, _) -> heads inner
        | _ -> None
      in
      let rec covering branches =
        match branches with
        | branch :: later ->
            let mask =
              match branch.coverage with
              | Total -> mask (written branch.outcome)
              | Partial ->
                  List.fold_left
                    (fun found (later : branch) ->
                      match (heads branch.pattern, heads later.pattern) with
                      | Some these, Some those when not (List.exists (fun head -> List.mem head those) these) -> found
                      | _, _ -> found lor mask (written later.outcome))
                    (mask (written branch.outcome)) later
            in
            case ~lhs:branch.pattern ~guard:None ~rhs:(eint ~loc mask) :: covering later
        | [] -> []
      in
      [%expr
        (fun __melhole_message -> [%e pexp_match ~loc [%expr __melhole_message] (covering branches)])
        [@ocaml.warning "-a"]])
    (branches expression)

let view expression =
  match expression.pexp_desc with
  | Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], _, Pfunction_body body) -> (
      match pattern.ppat_desc with
      | Ppat_any -> Some 0
      | Ppat_record (fields, _) -> Some (mask (Named (List.map (fun ({ txt; _ }, _) -> Longident.last_exn txt) fields)))
      | Ppat_var { txt = model; _ } ->
          let reader =
            object
              inherit [fields] Ast_traverse.fold as super

              method! expression expression found =
                match expression.pexp_desc with
                | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, { txt; _ })
                  when String.equal name model ->
                    union found (Named [ Longident.last_exn txt ])
                | Pexp_ident { txt = Lident name; _ } when String.equal name model -> Every
                | _ -> super#expression expression found

              method! pattern pattern found =
                match List.mem model (Melhole_compiler.Free.bound pattern) with true -> Every | false -> found
            end
          in
          Some (mask (reader#expression body (Named [])))
      | _ -> None)
  | _ -> None
