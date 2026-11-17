open Ppxlib

let bound =
  object
    inherit [string list] Ast_traverse.fold as super

    method! pattern pattern found =
      let found =
        match pattern.ppat_desc with
        | Ppat_var { txt; _ } | Ppat_alias (_, { txt; _ }) -> txt :: found
        | _ -> found
      in
      super#pattern pattern found
  end

let read =
  object (self)
    inherit [string list] Ast_traverse.fold as super

    method! expression expression found =
      match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
      | Pexp_ident { txt = Lident name; _ }, _ -> name :: found
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ }, arguments), Some _ ->
          List.fold_left (fun found (_, argument) -> self#expression argument found) found arguments
      | _ -> super#expression expression found
  end

let opens =
  object
    inherit [[ `Opens | `Closed ]] Ast_traverse.fold as super

    method! expression expression found =
      match expression.pexp_desc with
      | Pexp_open _ | Pexp_letmodule _ | Pexp_letexception _ -> `Opens
      | _ -> super#expression expression found
  end

let structure items =
  let rec atomic (argument : expression) =
    match argument.pexp_desc with
    | Pexp_constant _ | Pexp_ident _ | Pexp_construct (_, None) | Pexp_variant (_, None) -> `Atomic
    | Pexp_construct (_, Some inner) | Pexp_variant (_, Some inner) -> atomic inner
    | Pexp_tuple items ->
        List.fold_left (fun found item -> match found with `Atomic -> atomic item | `Compound -> `Compound) `Atomic items
    | _ -> `Compound
  in
  let helper (definition : expression) =
    match (definition.pexp_desc : expression_desc) with
    | Pexp_function ((_ :: _ as parameters), None, Pfunction_body body) -> (
        match (body.pexp_desc, Melhole_compiler.Element.without_jsx body.pexp_attributes) with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ }, _), Some _ ->
            Option.map
              (fun names -> (names, body))
              (List.fold_right
                 (fun parameter found ->
                   Option.bind found (fun names ->
                       match parameter.pparam_desc with
                       | Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt; _ }; _ }) -> Some (txt :: names)
                       | _ -> None))
                 parameters (Some []))
        | _ -> None)
    | _ -> None
  in
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        match expression.pexp_desc with
        | Pexp_let (Nonrecursive, ([ { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; pvb_expr; _ } ] as bindings), scope)
          -> (
            match helper pvb_expr with
            | None -> expression
            | Some (parameters, body) -> (
                let inside = bound#expression body [] and outside = bound#expression scope [] in
                let reads = List.filter (fun read -> not (List.mem read parameters)) (read#expression body []) in
                match
                  ( opens#expression scope (opens#expression body `Closed),
                    List.exists (fun read -> List.mem read outside) (name :: reads),
                    List.exists (fun parameter -> List.mem parameter inside) parameters )
                with
                | `Opens, _, _ | _, true, _ | _, _, true -> expression
                | `Closed, false, false -> (
                    let spliced (call : expression) =
                      match call.pexp_desc with
                      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident called; _ }; _ }, arguments)
                        when String.equal called name && List.compare_lengths arguments parameters = 0 ->
                          let given =
                            List.fold_right2
                              (fun parameter (label, (argument : expression)) found ->
                                Option.bind found (fun rest ->
                                    match (label, atomic argument, argument.pexp_desc) with
                                    | Nolabel, `Atomic, Pexp_ident { txt = Lident used; _ } when List.mem used inside -> None
                                    | Nolabel, `Atomic, _ -> Some ((parameter, argument) :: rest)
                                    | (Nolabel | Labelled _ | Optional _), _, _ -> None))
                              parameters arguments (Some [])
                          in
                          Option.fold ~none:call
                            ~some:(fun given ->
                              let put =
                                object (self)
                                  inherit Ast_traverse.map as super

                                  method! expression expression =
                                    match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
                                    | Pexp_ident { txt = Lident used; _ }, _ ->
                                        Option.value ~default:expression (List.assoc_opt used given)
                                    | Pexp_apply (({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ } as tag), arguments), Some _ ->
                                        {
                                          expression with
                                          pexp_desc =
                                            Pexp_apply (tag, List.map (fun (label, argument) -> (label, self#expression argument)) arguments);
                                        }
                                    | _ -> super#expression expression
                                end
                              in
                              put#expression body)
                            given
                      | _ -> call
                    in
                    let children =
                      object
                        inherit Ast_traverse.map as super

                        method! expression expression =
                          let expression = super#expression expression in
                          match (expression.pexp_desc, Melhole_compiler.Element.without_jsx expression.pexp_attributes) with
                          | Pexp_apply (callee, arguments), Some _ ->
                              let rec items (list : expression) =
                                match list.pexp_desc with
                                | Pexp_construct (({ txt = Lident "::"; _ } as cons), Some ({ pexp_desc = Pexp_tuple [ head; tail ]; _ } as pair)) ->
                                    {
                                      list with
                                      pexp_desc = Pexp_construct (cons, Some { pair with pexp_desc = Pexp_tuple [ spliced head; items tail ] });
                                    }
                                | _ -> list
                              in
                              {
                                expression with
                                pexp_desc =
                                  Pexp_apply
                                    ( callee,
                                      List.map
                                        (fun (label, argument) ->
                                          match label with
                                          | Labelled "children" -> (label, items argument)
                                          | Nolabel | Labelled _ | Optional _ -> (label, argument))
                                        arguments );
                              }
                          | _ -> expression
                      end
                    in
                    let scope = children#expression scope in
                    match List.mem name (read#expression scope []) with
                    | true -> { expression with pexp_desc = Pexp_let (Nonrecursive, bindings, scope) }
                    | false -> scope)))
        | _ -> expression
    end
  in
  mapper#structure items
