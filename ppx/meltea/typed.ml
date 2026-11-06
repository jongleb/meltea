open Ppxlib

let reads (binding : Ocaml_common.Typedtree.value_binding) =
  match binding.vb_expr.exp_desc with
  | Ocaml_common.Typedtree.Texp_function ([ { fp_kind = Ocaml_common.Typedtree.Tparam_pat pattern; _ } ], body) -> (
      match pattern.pat_desc with
      | Ocaml_common.Typedtree.Tpat_record (fields, _) -> List.map (fun ((name : Ocaml_common.Longident.t Ocaml_common.Location.loc), label, _) -> (name.loc, label)) fields
      | Ocaml_common.Typedtree.Tpat_var (model, _, _) ->
          let found = ref [] in
          let iterator =
            {
              Ocaml_common.Tast_iterator.default_iterator with
              expr =
                (fun self expression ->
                  (match expression.exp_desc with
                  | Ocaml_common.Typedtree.Texp_field
                      ({ exp_desc = Ocaml_common.Typedtree.Texp_ident (Ocaml_common.Path.Pident base, _, _); _ }, name, label)
                    when Ocaml_common.Ident.same base model ->
                      found := (name.loc, label) :: !found
                  | _ -> ());
                  Ocaml_common.Tast_iterator.default_iterator.expr self expression);
            }
          in
          (match body with
          | Ocaml_common.Typedtree.Tfunction_body body -> iterator.expr iterator body
          | Ocaml_common.Typedtree.Tfunction_cases _ -> ());
          List.rev !found
      | _ -> [])
  | _ -> []

let check structure =
  let views =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } ])
          when String.starts_with ~prefix:"__melhole_reads_" txt ->
            Some (String.sub txt 16 (String.length txt - 16))
        | _ -> None)
      structure
  in
  Option.fold ~none:[]
    ~some:(fun file ->
      List.concat_map
        (fun (item : Ocaml_common.Typedtree.structure_item) ->
          match item.str_desc with
          | Ocaml_common.Typedtree.Tstr_value (_, bindings) ->
              List.concat_map
                (fun (binding : Ocaml_common.Typedtree.value_binding) ->
                  match binding.vb_pat.pat_desc with
                  | Ocaml_common.Typedtree.Tpat_var (name, _, _) when List.mem (Ocaml_common.Ident.name name) views ->
                      List.filter_map
                        (fun (loc, (label : Ocaml_common.Data_types.label_description)) ->
                          match
                            ( label.lbl_mut,
                              Melhole_typed.Mutability.of_answer (Melhole_typed.Answer.make (Melhole_typed.File.env file) label.lbl_arg) )
                          with
                          | ( Ocaml_common.Asttypes.Immutable,
                              (Melhole_typed.Mutability.Immediate | Melhole_typed.Mutability.Immutable_boxed
                              | Melhole_typed.Mutability.Unknown) ) ->
                              None
                          | Ocaml_common.Asttypes.Mutable, _ | _, Melhole_typed.Mutability.Mutable_inside ->
                              Some [%stri let () = (Program.mutable_read [@ocaml.warning "-a"])])
                        (reads binding)
                  | _ -> [])
                bindings
          | _ -> [])
        (Melhole_typed.File.typed file).str_items)
    (Melhole_typed.File.of_structure structure)

let () =
  Driver.register_transformation "meltea-typed"
    ~instrument:
      (Driver.Instrument.make ~position:After (fun structure ->
           match Melhole_typed.Mode.current () with
           | Melhole_typed.Mode.Untyped -> structure
           | Melhole_typed.Mode.Typed -> structure @ check structure))
