open Ocaml_common

type t = { env : Env.t; type_expr : Types.type_expr }
type label = Unlabelled | Labelled of string | Optional of string
type representation = Abstract | Unboxed of t | Defined
type row = Closed | Open

type view =
  | Function of label * t * t
  | Named of { path : string; arguments : t list; representation : representation }
  | Js_object of { fields : (string * t) list; row : row }
  | Other

let make env type_expr = { env; type_expr }

let name env path = Path.name (match Env.normalize_type_path None env path with exception Not_found -> path | normal -> normal)

let rec view { env; type_expr } =
  let within type_expr = { env; type_expr } in
  match Types.get_desc (Ctype.expand_head_opt env type_expr) with
  | Types.Tarrow (label, parameter, result, _) ->
      Function
        ( (match label with
          | Asttypes.Nolabel -> Unlabelled
          | Asttypes.Labelled name -> Labelled name
          | Asttypes.Optional name -> Optional name),
          within parameter,
          within result )
  | Types.Tpoly (inner, []) -> view (within inner)
  | Types.Tconstr (path, [ fields ], _) when String.equal (name env path) "Js.t" -> (
      let shape = Ctype.expand_head_opt env fields in
      match Types.get_desc shape with
      | Types.Tobject _ ->
          let listed, rest = Ctype.flatten_fields (Ctype.object_fields shape) in
          Js_object
            {
              fields =
                List.filter_map
                  (fun (name, kind, field) ->
                    match Types.field_kind_repr kind with
                    | Types.Fpublic -> Some (name, within field)
                    | Types.Fabsent | Types.Fprivate -> None)
                  listed;
              row = (match Types.get_desc rest with Types.Tnil -> Closed | _ -> Open);
            }
      | _ -> Other)
  | Types.Tconstr (path, arguments, _) ->
      let representation =
        match Env.find_type path env with
        | exception Not_found -> Abstract
        | declaration -> (
            match declaration.type_kind with
            | Types.Type_abstract _ | Types.Type_external _ -> Abstract
            | Types.Type_variant ([ { cd_args = Types.Cstr_tuple [ held ]; _ } ], Types.Variant_unboxed)
            | Types.Type_record ([ { ld_type = held; _ } ], Types.Record_unboxed _) -> (
                match Ctype.apply env declaration.type_params held arguments with
                | exception Ctype.Cannot_apply -> Abstract
                | applied -> Unboxed (within applied))
            | Types.Type_variant _ | Types.Type_record _ | Types.Type_open -> Defined)
      in
      Named { path = name env path; arguments = List.map within arguments; representation }
  | Types.Tvar _ | Types.Tunivar _ | Types.Ttuple _ | Types.Tvariant _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
  | Types.Tlink _ | Types.Tsubst _ | Types.Tpoly _ | Types.Tpackage _ | Types.Tfunctor _ ->
      Other
