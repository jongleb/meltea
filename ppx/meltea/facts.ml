open Ppxlib

(** Where a write made by a [mapi] lands, as a constant or as an expression of the source. *)
type place = Constant of int | Source of Location.t

(** What an application proves about its result and the collection it was made from. *)
type made = Appended | Cut of Location.t * Location.t | Point of place | Strided of int * int

(** What writes to a copied array prove about it and the array it copies. *)
type written = Exchanged of Location.t * Location.t | Rewritten of Location.t list

(** A recorded fact: on the result of an application, or on a copy once it is written. *)
type site =
  | Made of { before : Location.t; made : made }
  | Copied of { before : Location.t; copy : string; writes : int; written : written }

(** Whether two pure expressions are the same expression. *)
type likeness = Same | Different

(** Whether an expression mentions a name. *)
type reference = Refers | Never_refers

(** How what follows the writes to a copied array uses it: only stores it, or may write or pass it on. *)
type use = Stored | Escapes

let structure file structure =
  let named path =
    let name = Ocaml_common.Path.name path in
    List.fold_left
      (fun name prefix ->
        match String.starts_with ~prefix name with
        | true -> String.sub name (String.length prefix) (String.length name - String.length prefix)
        | false -> name)
      name [ "Stdlib."; "Stdlib__" ]
    |> String.split_on_char '.'
    |> List.map (fun part -> match String.starts_with ~prefix:"\\#" part with true -> String.sub part 2 (String.length part - 2) | false -> part)
    |> String.concat "."
  in
  let called (expression : Ocaml_common.Typedtree.expression) names arity =
    match expression.exp_desc with
    | Texp_apply ({ exp_desc = Texp_ident (path, _, _); _ }, arguments)
      when List.mem (named path) names && List.length arguments = arity ->
        List.fold_right
          (fun (label, argument) found ->
            Option.bind found (fun rest ->
                match (label, argument) with
                | Ocaml_common.Asttypes.Nolabel, Ocaml_common.Typedtree.Arg value -> Some (value :: rest)
                | _ -> None))
          arguments (Some [])
    | _ -> None
  in
  let rec pure (expression : Ocaml_common.Typedtree.expression) =
    match expression.exp_desc with
    | Texp_ident _ | Texp_constant _ -> Some expression
    | Texp_field (inner, _, { lbl_mut = Immutable; _ }) -> Option.map (fun _ -> expression) (pure inner)
    | Texp_apply _ ->
        Option.bind
          (match called expression [ "+"; "-"; "*"; "/"; "mod" ] 2 with
          | Some _ as found -> found
          | None -> called expression [ "Array.length" ] 1)
          (fun arguments ->
            Option.map
              (fun () -> expression)
              (List.fold_left (fun found argument -> Option.bind found (fun () -> Option.map ignore (pure argument))) (Some ()) arguments))
    | _ -> None
  in
  let rec same (first : Ocaml_common.Typedtree.expression) (second : Ocaml_common.Typedtree.expression) =
    match (first.exp_desc, second.exp_desc) with
    | Texp_ident (one, _, _), Texp_ident (other, _, _) -> ( match Ocaml_common.Path.same one other with true -> Same | false -> Different)
    | Texp_constant (Const_int one), Texp_constant (Const_int other) -> ( match Int.equal one other with true -> Same | false -> Different)
    | Texp_field (one, _, one_label), Texp_field (other, _, other_label) -> (
        match String.equal one_label.lbl_name other_label.lbl_name with true -> same one other | false -> Different)
    | Texp_apply (one, one_arguments), Texp_apply (other, other_arguments) when List.length one_arguments = List.length other_arguments ->
        List.fold_left2
          (fun found (_, one) (_, other) ->
            match (found, one, other) with
            | Same, Ocaml_common.Typedtree.Arg one, Ocaml_common.Typedtree.Arg other -> same one other
            | _ -> Different)
          (same one other) one_arguments other_arguments
    | _ -> Different
  in
  let mentions ident (expression : Ocaml_common.Typedtree.expression) =
    let found = ref Never_refers in
    let iterator =
      {
        Ocaml_common.Tast_iterator.default_iterator with
        expr =
          (fun self expression ->
            (match expression.exp_desc with
            | Texp_ident (Pident name, _, _) when Ocaml_common.Ident.same name ident -> found := Refers
            | _ -> ());
            Ocaml_common.Tast_iterator.default_iterator.expr self expression);
      }
    in
    iterator.expr iterator expression;
    !found
  in
  let variable (expression : Ocaml_common.Typedtree.expression) ident =
    match expression.exp_desc with
    | Texp_ident (Pident name, _, _) when Ocaml_common.Ident.same name ident -> Some ()
    | _ -> None
  in
  let integer (expression : Ocaml_common.Typedtree.expression) =
    match expression.exp_desc with Texp_constant (Const_int value) -> Some value | _ -> None
  in
  let appended (expression : Ocaml_common.Typedtree.expression) =
    Option.bind (called expression [ "Array.append" ] 2) (function
      | [ first; second ] -> (
          let cut =
            Option.bind (called first [ "Array.sub" ] 3) (fun kept ->
                Option.bind (called second [ "Array.sub" ] 3) (fun rest ->
                    match (kept, rest) with
                    | [ source; start; until ], [ other; from; _ ] -> (
                        match (pure source, integer start, pure until, pure from, same source other) with
                        | Some _, Some 0, Some _, Some _, Same -> Some (Made { before = source.exp_loc; made = Cut (until.exp_loc, from.exp_loc) })
                        | _ -> None)
                    | _ -> None))
          in
          match cut with
          | Some _ -> cut
          | None -> Option.map (fun before -> Made { before = before.Ocaml_common.Typedtree.exp_loc; made = Appended }) (pure first))
      | _ -> None)
  in
  let mapped (expression : Ocaml_common.Typedtree.expression) =
    Option.bind (called expression [ "Array.mapi" ] 2) (function
      | [ { exp_desc = Texp_function ([ index; row ], Tfunction_body body); _ }; source ] -> (
          match (index.fp_kind, row.fp_kind, pure source) with
          | ( Tparam_pat { pat_desc = Tpat_var (index, _, _); _ },
              Tparam_pat { pat_desc = Tpat_var (row, _, _); _ },
              Some source ) -> (
              let outside (place : Ocaml_common.Typedtree.expression) =
                Option.bind (pure place) (fun place ->
                    match (mentions index place, mentions row place) with
                    | Never_refers, Never_refers -> Some (Source place.exp_loc)
                    | _ -> None)
              in
              let modulo (tested : Ocaml_common.Typedtree.expression) =
                Option.bind (called tested [ "mod" ] 2) (function
                  | [ left; right ] ->
                      Option.bind (variable left index) (fun () ->
                          Option.bind (integer right) (fun step -> match step > 0 with true -> Some step | false -> None))
                  | _ -> None)
              in
              let tested (scrutinee : Ocaml_common.Typedtree.expression) constant =
                match (variable scrutinee index, modulo scrutinee) with
                | Some (), _ -> Some (Point (Constant constant))
                | None, Some step -> ( match constant >= 0 with true -> Some (Strided (step, constant)) | false -> None)
                | None, None -> None
              in
              let condition (test : Ocaml_common.Typedtree.expression) =
                Option.bind (called test [ "=" ] 2) (function
                  | [ left; right ] -> (
                      let sided (first : Ocaml_common.Typedtree.expression) second =
                        match (variable first index, modulo first) with
                        | Some (), _ -> Option.map (fun place -> Point place) (outside second)
                        | None, Some step -> Option.bind (integer second) (fun from -> match from >= 0 with true -> Some (Strided (step, from)) | false -> None)
                        | None, None -> None
                      in
                      match sided left right with Some _ as found -> found | None -> sided right left)
                  | _ -> None)
              in
              let made =
                match body.exp_desc with
                | Texp_ifthenelse (test, _, Some otherwise) -> Option.bind (variable otherwise row) (fun () -> condition test)
                | Texp_match (scrutinee, [ written; kept ], [], _) -> (
                    match (written.c_lhs.pat_desc, written.c_guard, kept.c_lhs.pat_desc, kept.c_guard) with
                    | Tpat_value first, None, Tpat_value second, None -> (
                        match
                          ( (first :> Ocaml_common.Typedtree.pattern).pat_desc,
                            (second :> Ocaml_common.Typedtree.pattern).pat_desc )
                        with
                        | Tpat_constant (Const_int constant), Tpat_any ->
                            Option.bind (variable kept.c_rhs row) (fun () -> tested scrutinee constant)
                        | _ -> None)
                    | _ -> None)
                | _ -> None
              in
              Option.map (fun made -> Made { before = source.exp_loc; made }) made)
          | _ -> None)
      | _ -> None)
  in
  let copied (expression : Ocaml_common.Typedtree.expression) =
    match expression.exp_desc with
    | Texp_let (Nonrecursive, [ { vb_pat = { pat_desc = Tpat_var (copy, name, _); _ }; vb_expr; _ } ], body) ->
        Option.bind (called vb_expr [ "Array.copy" ] 1) (fun arguments ->
            Option.bind (pure (List.hd arguments)) (fun source ->
                let written (write : Ocaml_common.Typedtree.expression) =
                  Option.bind (called write [ "Array.set"; "Array.unsafe_set" ] 3) (function
                    | [ target; at; value ] ->
                        Option.bind (variable target copy) (fun () ->
                            Option.bind (pure at) (fun at ->
                                match mentions copy value with Never_refers -> Some (at, value) | Refers -> None))
                    | _ -> None)
                in
                let rec writes (expression : Ocaml_common.Typedtree.expression) =
                  match expression.exp_desc with
                  | Texp_sequence (write, rest) ->
                      Option.fold ~none:([], expression)
                        ~some:(fun found ->
                          let more, rest = writes rest in
                          (found :: more, rest))
                        (written write)
                  | _ -> ([], expression)
                in
                let rec use (expression : Ocaml_common.Typedtree.expression) =
                  let both first second = match (first, second) with Stored, Stored -> Stored | (Stored | Escapes), _ -> Escapes in
                  let every uses = List.fold_left both Stored uses in
                  let unmentioned found = match mentions copy found with Never_refers -> Stored | Refers -> Escapes in
                  match expression.exp_desc with
                  | Texp_ident _ -> Stored
                  | Texp_record { fields; extended_expression; _ } ->
                      every
                        (Option.fold ~none:Stored ~some:unmentioned extended_expression
                        :: List.map
                             (fun (_, definition) ->
                               match definition with
                               | Ocaml_common.Typedtree.Overridden (_, value) -> use value
                               | Ocaml_common.Typedtree.Kept _ -> Stored)
                             (Array.to_list fields))
                  | Texp_tuple values -> every (List.map (fun (_, value) -> use value) values)
                  | Texp_construct (_, _, values) -> every (List.map use values)
                  | Texp_sequence (first, rest) -> both (unmentioned first) (use rest)
                  | Texp_let (_, bindings, rest) -> every (use rest :: List.map (fun binding -> unmentioned binding.Ocaml_common.Typedtree.vb_expr) bindings)
                  | Texp_ifthenelse (condition, yes, no) -> every [ unmentioned condition; use yes; Option.fold ~none:Stored ~some:use no ]
                  | Texp_match (scrutinee, cases, [], _) ->
                      every
                        (unmentioned scrutinee
                        :: List.map
                             (fun (case : Ocaml_common.Typedtree.computation Ocaml_common.Typedtree.case) ->
                               both (Option.fold ~none:Stored ~some:unmentioned case.c_guard) (use case.c_rhs))
                             cases)
                  | _ -> unmentioned expression
                in
                let found, rest = writes body in
                match (found, use rest) with
                | [], _ | _, Escapes -> None
                | _ :: _, Stored ->
                    let read (value : Ocaml_common.Typedtree.expression) =
                      Option.bind (called value [ "Array.get"; "Array.unsafe_get" ] 2) (function
                        | [ array; from ] -> ( match same array source with Same -> Some from | Different -> None)
                        | _ -> None)
                    in
                    let written =
                      match List.map (fun (at, value) -> (at, read value)) found with
                      | [ (first, Some first_from); (second, Some second_from) ] -> (
                          match (same first_from second, same second_from first) with
                          | Same, Same -> Exchanged (first.exp_loc, second.exp_loc)
                          | _ -> Rewritten [ first.exp_loc; second.exp_loc ])
                      | _ -> Rewritten (List.map (fun ((at : Ocaml_common.Typedtree.expression), _) -> at.exp_loc) found)
                    in
                    Some (Copied { before = source.exp_loc; copy = name.txt; writes = List.length found; written })))
    | _ -> None
  in
  let sites = Hashtbl.create 16 in
  let iterator =
    {
      Ocaml_common.Tast_iterator.default_iterator with
      expr =
        (fun self expression ->
          Option.iter
            (fun site -> Hashtbl.replace sites expression.exp_loc site)
            (List.find_map (fun recognise -> recognise expression) [ appended; mapped; copied ]);
          Ocaml_common.Tast_iterator.default_iterator.expr self expression);
    }
  in
  iterator.structure iterator (Melhole_typed.File.typed file);
  let source loc (expression : expression) =
    let finder =
      object
        inherit [expression option] Ast_traverse.fold as super

        method! expression expression found =
          match found with
          | Some _ -> found
          | None -> (
              match Location.compare expression.pexp_loc loc with
              | 0 -> Some expression
              | _ -> super#expression expression found)
      end
    in
    finder#expression expression None
  in
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        let loc = expression.pexp_loc in
        let recorded kind before after = [%expr Melhole_meltea.Delta.record [%e kind] [%e before] [%e after]] in
        match (Hashtbl.find_opt sites loc, expression.pexp_desc) with
        | Some (Made { before; made }), Pexp_apply _ ->
            let at place = source place expression in
            let kind =
              match made with
              | Appended -> Some [%expr Melhole_meltea.Delta.appended]
              | Cut (until, from) ->
                  Option.bind (at until) (fun until -> Option.map (fun from -> [%expr Melhole_meltea.Delta.cut [%e until] [%e from]]) (at from))
              | Point (Constant place) -> Some [%expr Melhole_meltea.Delta.rewritten [| [%e Ast_builder.Default.eint ~loc place] |]]
              | Point (Source place) -> Option.map (fun place -> [%expr Melhole_meltea.Delta.rewritten [| [%e place] |]]) (at place)
              | Strided (step, from) ->
                  Some [%expr Melhole_meltea.Delta.strided [%e Ast_builder.Default.eint ~loc step] [%e Ast_builder.Default.eint ~loc from]]
            in
            Option.value ~default:expression
              (Option.bind kind (fun kind -> Option.map (fun before -> recorded kind before expression) (at before)))
        | Some (Copied { before; copy; writes; written }), Pexp_let (Nonrecursive, [ binding ], body) ->
            let at place = source place expression in
            let kind =
              match written with
              | Exchanged (first, second) ->
                  Option.bind (at first) (fun first -> Option.map (fun second -> [%expr Melhole_meltea.Delta.exchanged [%e first] [%e second]]) (at second))
              | Rewritten places ->
                  Option.map
                    (fun places -> [%expr Melhole_meltea.Delta.rewritten [%e Ast_builder.Default.pexp_array ~loc places]])
                    (List.fold_right (fun place found -> Option.bind found (fun rest -> Option.map (fun place -> place :: rest) (at place))) places (Some []))
            in
            let rec inserted count (body : expression) note =
              match (count, body.pexp_desc) with
              | 0, _ -> Some (Ast_builder.Default.pexp_sequence ~loc note body)
              | _, Pexp_sequence (write, rest) ->
                  Option.map (fun rest -> { body with pexp_desc = Pexp_sequence (write, rest) }) (inserted (count - 1) rest note)
              | _ -> None
            in
            Option.value ~default:expression
              (Option.bind kind (fun kind ->
                   Option.bind (at before) (fun before ->
                       Option.map
                         (fun body -> { expression with pexp_desc = Pexp_let (Nonrecursive, [ binding ], body) })
                         (inserted writes body [%expr ignore [%e recorded kind before (Ast_builder.Default.evar ~loc copy)]]))))
        | _ -> expression
    end
  in
  match !Melhole_compiler.Element.output with
  | Melhole_compiler.Element.Client_only | Melhole_compiler.Element.Hydratable -> mapper#structure structure
  | Melhole_compiler.Element.Server -> structure
