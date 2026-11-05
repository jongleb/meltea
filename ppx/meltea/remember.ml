open Ppxlib
open Ast_builder.Default

let remembered ~model ~locals binding =
  let expression = binding.pvb_expr in
  let loc = expression.pexp_loc in
  let inspect =
    object
      inherit [[ `Plain | `Calls | `Visual ] * expression list * [ `Fields | `Whole ]] Ast_traverse.fold as super

      method! expression expression ((kind, fields, use) as found) =
        match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
        | _, Some _ -> (`Visual, fields, use)
        | Pexp_ident { txt = Ldot (Lident "Html", _); _ }, None -> (`Visual, fields, use)
        | Pexp_field ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _), None
          when Option.fold ~none:false ~some:(String.equal name) model ->
            (kind, expression :: fields, use)
        | Pexp_ident { txt = Lident name; _ }, None when Option.fold ~none:false ~some:(String.equal name) model ->
            (kind, fields, `Whole)
        | Pexp_apply _, None ->
            super#expression expression ((match kind with `Plain -> `Calls | (`Calls | `Visual) as kept -> kept), fields, use)
        | _ -> super#expression expression found
    end
  in
  match (expression.pexp_desc, inspect#expression expression (`Plain, [], `Fields), Melhole_compiler.Free.of_expression expression) with
  | Pexp_function _, _, _ | _, ((`Plain | `Visual), _, _), _ | _, _, Melhole_compiler.Free.Volatile -> binding
  | _, (`Calls, fields, use), Melhole_compiler.Free.Reads names ->
      let named =
        List.filter
          (fun name -> List.mem name locals && not (Option.fold ~none:false ~some:(String.equal name) model))
          names
      in
      let whole = match (use, model) with `Whole, Some model -> [ evar ~loc model ] | (`Whole | `Fields), _ -> [] in
      let fields =
        List.sort_uniq
          (fun first second -> compare (Pprintast.string_of_expression first) (Pprintast.string_of_expression second))
          fields
      in
      (match whole @ fields @ List.map (evar ~loc) named with
      | [] -> binding
      | inputs ->
          let memory = Melhole_compiler.Hoist.hoist ~loc "memory" [%expr Melhole.View.memory ()] in
          {
            binding with
            pvb_expr =
              [%expr
                Melhole.View.remember [%e memory]
                  [%e pexp_array ~loc (List.map (fun input -> [%expr Melhole.View.Input [%e input]]) inputs)]
                  (fun () -> [%e expression])];
          })

let structure_item item =
  match item.pstr_desc with
  | Pstr_value (Nonrecursive, [ ({ pvb_pat = { ppat_desc = Ppat_var { txt = "view"; _ }; _ }; pvb_expr; _ } as view) ]) -> (
      match pvb_expr.pexp_desc with
      | Pexp_function (([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ] as parameters), None, Pfunction_body body) ->
          let model = match pattern.ppat_desc with Ppat_var { txt; _ } -> Some txt | _ -> None in
          let rec spine locals body =
            match body.pexp_desc with
            | Pexp_let (Nonrecursive, [ binding ], rest) ->
                {
                  body with
                  pexp_desc =
                    Pexp_let
                      ( Nonrecursive,
                        [ remembered ~model ~locals binding ],
                        spine (Melhole_compiler.Free.bound binding.pvb_pat @ locals) rest );
                }
            | _ -> body
          in
          let locals = Option.to_list model @ Melhole_compiler.Free.bound pattern in
          {
            item with
            pstr_desc =
              Pstr_value
                ( Nonrecursive,
                  [ { view with pvb_expr = { pvb_expr with pexp_desc = Pexp_function (parameters, None, Pfunction_body (spine locals body)) } } ] );
          }
      | _ -> item)
  | _ -> item
