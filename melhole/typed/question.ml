type t = Asked of int | Unasked

let name = "melhole.ask"
let asked = ref 0

let marked loc attributes =
  match Mode.current () with
  | Mode.Untyped -> (Unasked, attributes)
  | Mode.Typed ->
      incr asked;
      (Asked !asked, Mark.make loc name !asked :: attributes)

let expression (expression : Ppxlib.expression) =
  let question, attributes = marked expression.pexp_loc expression.pexp_attributes in
  (question, { expression with pexp_attributes = attributes })

let value (description : Ppxlib.value_description) =
  let question, attributes = marked description.pval_loc description.pval_attributes in
  (question, { description with pval_attributes = attributes })

let of_attributes attributes =
  List.find_map
    (fun (attribute : Ocaml_common.Typedtree.attribute) ->
      match (String.equal attribute.attr_name.txt name, attribute.attr_payload) with
      | true, PStr payload ->
          Option.map (fun number -> Asked number) (Mark.number (Ppxlib.PStr (Ppxlib_ast.Selected_ast.of_ocaml Structure payload)))
      | _ -> None)
    attributes

let unmarked attributes = Mark.without name attributes
