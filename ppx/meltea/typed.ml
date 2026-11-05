open Ppxlib
module Typed = Ocaml_common.Typedtree

let enabled = ref false

let () =
  Driver.add_arg "-typed" (Arg.Set enabled)
    ~doc:" Type the file after rewriting it and warn where a view reads a model field that can change in place"

let rec changes_in_place env seen ty =
  let open Ocaml_common in
  match Types.get_desc (Ctype.expand_head env ty) with
  | Types.Ttuple items -> changes_in_place_all env seen (List.map snd items)
  | Types.Tconstr (path, arguments, _) -> (
      match (Path.same path Predef.path_array || Path.same path Predef.path_bytes, changes_in_place_all env seen arguments) with
      | true, _ | _, Ocaml_common.Asttypes.Mutable -> Ocaml_common.Asttypes.Mutable
      | false, Ocaml_common.Asttypes.Immutable when List.exists (Path.same path) seen -> Ocaml_common.Asttypes.Immutable
      | false, Ocaml_common.Asttypes.Immutable -> (
          let labels declarations =
            match
              List.exists
                (fun (label : Types.label_declaration) ->
                  label.ld_mutable = Ocaml_common.Asttypes.Mutable
                  || changes_in_place env (path :: seen) label.ld_type = Ocaml_common.Asttypes.Mutable)
                declarations
            with
            | true -> Ocaml_common.Asttypes.Mutable
            | false -> Ocaml_common.Asttypes.Immutable
          in
          match (Env.find_type path env).type_kind with
          | Types.Type_record (declarations, _) -> labels declarations
          | Types.Type_variant (constructors, _) ->
              List.fold_left
                (fun found (constructor : Types.constructor_declaration) ->
                  match (found, constructor.cd_args) with
                  | Ocaml_common.Asttypes.Mutable, _ -> Ocaml_common.Asttypes.Mutable
                  | Ocaml_common.Asttypes.Immutable, Types.Cstr_tuple types -> changes_in_place_all env (path :: seen) types
                  | Ocaml_common.Asttypes.Immutable, Types.Cstr_record declarations -> labels declarations)
                Ocaml_common.Asttypes.Immutable constructors
          | Types.Type_abstract _ | Types.Type_open | Types.Type_external _ -> Ocaml_common.Asttypes.Immutable
          | exception Not_found -> Ocaml_common.Asttypes.Immutable))
  | _ -> Ocaml_common.Asttypes.Immutable

and changes_in_place_all env seen types =
  match List.exists (fun ty -> changes_in_place env seen ty = Ocaml_common.Asttypes.Mutable) types with
  | true -> Ocaml_common.Asttypes.Mutable
  | false -> Ocaml_common.Asttypes.Immutable

let reads (binding : Typed.value_binding) =
  match binding.vb_expr.exp_desc with
  | Typed.Texp_function ([ { fp_kind = Typed.Tparam_pat pattern; _ } ], body) -> (
      match pattern.pat_desc with
      | Typed.Tpat_record (fields, _) -> List.map (fun ((name : Ocaml_common.Longident.t Ocaml_common.Location.loc), label, _) -> (name.loc, label)) fields
      | Typed.Tpat_var (model, _, _) ->
          let found = ref [] in
          let iterator =
            {
              Ocaml_common.Tast_iterator.default_iterator with
              expr =
                (fun self expression ->
                  (match expression.exp_desc with
                  | Typed.Texp_field
                      ({ exp_desc = Typed.Texp_ident (Ocaml_common.Path.Pident base, _, _); _ }, name, label)
                    when Ocaml_common.Ident.same base model ->
                      found := (name.loc, label) :: !found
                  | _ -> ());
                  Ocaml_common.Tast_iterator.default_iterator.expr self expression);
            }
          in
          (match body with
          | Typed.Tfunction_body body -> iterator.expr iterator body
          | Typed.Tfunction_cases _ -> ());
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
  let typable =
    object
      inherit Ast_traverse.map as super

      method! structure_item item =
        match item.pstr_desc with
        | Pstr_primitive ({ pval_type = { ptyp_desc = Ptyp_arrow _; _ }; _ }) -> item
        | Pstr_primitive description ->
            let loc = item.pstr_loc in
            [%stri
              let [%p
                    Ast_builder.Default.ppat_constraint ~loc
                      (Ast_builder.Default.pvar ~loc description.pval_name.txt)
                      description.pval_type] =
                assert false]
        | _ -> super#structure_item item
    end
  in
  ignore (Ocaml_common.Warnings.parse_options false "-a");
  ignore (Ocaml_common.Warnings.parse_alert_option "-all");
  match
    Ocaml_common.Typemod.type_structure
      (Ocaml_common.Compmisc.initial_env ())
      (Ppxlib_ast.Selected_ast.to_ocaml Structure (typable#structure structure))
  with
  | exception _ -> []
  | typed, _, _, _, env ->
      List.concat_map
        (fun (item : Typed.structure_item) ->
          match item.str_desc with
          | Typed.Tstr_value (_, bindings) ->
              List.concat_map
                (fun (binding : Typed.value_binding) ->
                  match binding.vb_pat.pat_desc with
                  | Typed.Tpat_var (name, _, _) when List.mem (Ocaml_common.Ident.name name) views ->
                      List.filter_map
                        (fun (loc, (label : Ocaml_common.Data_types.label_description)) ->
                          match (label.lbl_mut, changes_in_place env [] label.lbl_arg) with
                          | Ocaml_common.Asttypes.Immutable, Ocaml_common.Asttypes.Immutable -> None
                          | Ocaml_common.Asttypes.Mutable, _ | _, Ocaml_common.Asttypes.Mutable ->
                              Some [%stri let () = (Program.mutable_read [@ocaml.warning "-a"])])
                        (reads binding)
                  | _ -> [])
                bindings
          | _ -> [])
        typed.str_items

let () =
  Driver.register_transformation "meltea-typed"
    ~instrument:
      (Driver.Instrument.make ~position:After (fun structure ->
           match !enabled with false -> structure | true -> structure @ check structure))
