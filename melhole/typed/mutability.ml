open Ocaml_common

type t = Immediate | Immutable_boxed | Mutable_inside | Unknown

let of_answer ({ env; type_expr } : Answer.t) =
  let boxed parts =
    List.fold_left
      (fun found part ->
        match (found, part) with
        | Mutable_inside, _ | _, Mutable_inside -> Mutable_inside
        | Unknown, _ | _, Unknown -> Unknown
        | (Immediate | Immutable_boxed), (Immediate | Immutable_boxed) -> Immutable_boxed)
      Immutable_boxed parts
  in
  let constructed parts = match parts with [] -> Immediate | _ :: _ -> boxed parts in
  let changing_in_place =
    [
      "Js.dict"; "Js.re"; "Js.iterator"; "Js.arrayBuffer"; "Js.int8Array"; "Js.uint8Array"; "Js.uint8ClampedArray";
      "Js.int16Array"; "Js.uint16Array"; "Js.int32Array"; "Js.uint32Array"; "Js.float32Array"; "Js.float64Array";
      "Js__Js_date.t"; "Stdlib__Hashtbl.t"; "Stdlib__Buffer.t"; "Stdlib__Queue.t"; "Stdlib__Stack.t"; "Stdlib__Atomic.t";
      "Stdlib__Weak.t";
    ]
  in
  let rec classify seen type_expr =
    match Types.get_desc (Ctype.expand_head_opt env type_expr) with
    | Types.Tarrow _ | Types.Tfunctor _ -> Immutable_boxed
    | Types.Ttuple items -> boxed (List.map (fun (_, item) -> classify seen item) items)
    | Types.Tpoly (inner, _) -> classify seen inner
    | Types.Tvariant row -> (
        match Types.row_closed row with
        | false -> Unknown
        | true ->
            constructed
              (List.concat_map
                 (fun (_, field) ->
                   match Types.row_field_repr field with
                   | Types.Rpresent None | Types.Rabsent -> []
                   | Types.Rpresent (Some held) -> [ classify seen held ]
                   | Types.Reither (_, held, _) -> List.map (classify seen) held)
                 (Types.row_fields row)))
    | Types.Tconstr (path, arguments, _) -> named seen path arguments
    | Types.Tvar _ | Types.Tunivar _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tlink _ | Types.Tsubst _
    | Types.Tpackage _ ->
        Unknown
  and named seen path arguments =
    let predefined =
      [
        (Predef.path_int, Immediate); (Predef.path_char, Immediate); (Predef.path_bool, Immediate);
        (Predef.path_unit, Immediate); (Predef.path_string, Immediate); (Predef.path_float, Immediate);
        (Predef.path_int32, Immediate); (Predef.path_nativeint, Immediate); (Predef.path_int64, Immutable_boxed);
        (Predef.path_array, Mutable_inside); (Predef.path_bytes, Mutable_inside); (Predef.path_floatarray, Mutable_inside);
        (Predef.path_exn, Unknown); (Predef.path_lazy_t, Unknown); (Predef.path_eff, Unknown);
        (Predef.path_continuation, Unknown); (Predef.path_extension_constructor, Immutable_boxed);
      ]
    in
    match List.find_map (fun (known, found) -> match Path.same known path with true -> Some found | false -> None) predefined with
    | Some found -> found
    | None when Path.same path Predef.path_iarray -> boxed (List.map (classify seen) arguments)
    | None when String.equal (Answer.name env path) "Js.t" -> (
        match Answer.view (Answer.make env (Ctype.newconstr path arguments)) with
        | Answer.Js_object { fields; row } ->
            boxed
              ((match row with Answer.Closed -> Immutable_boxed | Answer.Open -> Unknown)
              :: List.map
                   (fun (name, (field : Answer.t)) ->
                     match String.ends_with ~suffix:"#=" name with
                     | true -> Mutable_inside
                     | false -> classify seen field.type_expr)
                   fields)
        | Answer.Function _ | Answer.Named _ | Answer.Other -> Unknown)
    | None when List.exists (fun (visited, given) -> Path.same visited path && List.equal Types.eq_type given arguments) seen ->
        Immutable_boxed
    | None
      when List.exists (fun (visited, _) -> Path.same visited path) seen
           &&
           match Env.find_type path env with
           | { type_kind = Types.Type_record ([ _ ], Types.Record_unboxed _) | Types.Type_variant ([ _ ], Types.Variant_unboxed); _ } ->
               false
           | _ -> true
           | exception Not_found -> true ->
        boxed (List.map (classify seen) arguments)
    | None -> (
        match Env.find_type path env with
        | exception Not_found -> Unknown
        | declaration -> (
            let seen = (path, arguments) :: seen in
            let instance held =
              match Ctype.apply env declaration.type_params held arguments with
              | exception Ctype.Cannot_apply -> Unknown
              | applied -> classify seen applied
            in
            let fields labels =
              boxed
                (List.map
                   (fun (label : Types.label_declaration) ->
                     match (label.ld_mutable, label.ld_atomic) with
                     | Asttypes.Mutable, _ | _, Asttypes.Atomic -> Mutable_inside
                     | Asttypes.Immutable, Asttypes.Nonatomic -> instance label.ld_type)
                   labels)
            in
            match declaration.type_kind with
            | Types.Type_abstract _ | Types.Type_external _ -> (
                match
                  List.exists
                    (String.equal (Answer.name env path))
                    changing_in_place
                with
                | true -> Mutable_inside
                | false -> Unknown)
            | Types.Type_open -> Unknown
            | Types.Type_record ([ label ], Types.Record_unboxed _) -> instance label.ld_type
            | Types.Type_record (labels, _) -> fields labels
            | Types.Type_variant ([ { cd_args = Types.Cstr_tuple [ held ]; _ } ], Types.Variant_unboxed) -> instance held
            | Types.Type_variant ([ { cd_args = Types.Cstr_record [ label ]; _ } ], Types.Variant_unboxed) ->
                instance label.ld_type
            | Types.Type_variant (constructors, _) ->
                constructed
                  (List.concat_map
                     (fun (constructor : Types.constructor_declaration) ->
                       match (constructor.cd_args, constructor.cd_res) with
                       | Types.Cstr_tuple [], _ -> []
                       | (Types.Cstr_tuple _ | Types.Cstr_record _), Some _ -> [ Unknown ]
                       | Types.Cstr_tuple held, None -> List.map instance held
                       | Types.Cstr_record labels, None -> [ fields labels ])
                     constructors)))
  in
  classify [] type_expr
