open Ppxlib

let make loc name number =
  Ast_builder.Default.attribute ~loc ~name:{ loc; txt = name }
    ~payload:(PStr [ Ast_builder.Default.pstr_eval ~loc (Ast_builder.Default.eint ~loc number) [] ])

let number payload =
  match payload with
  | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_integer (digits, None)); _ }, _); _ } ] ->
      int_of_string_opt digits
  | PStr _ | PSig _ | PTyp _ | PPat _ -> None

let find name attributes =
  List.find_map
    (fun attribute ->
      match String.equal attribute.attr_name.txt name with
      | true -> number attribute.attr_payload
      | false -> None)
    attributes

let without name attributes = List.filter (fun attribute -> not (String.equal attribute.attr_name.txt name)) attributes
