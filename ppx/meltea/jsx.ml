open Ppxlib
open Melhole_compiler

let html_text expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Ldot (Lident "Html", "none"); _ } -> Some Element.Nothing
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", name); _ }; _ }, [ (Nolabel, value) ])
    -> (
      match name with
      | "text" -> Some (Element.Text (String_text, value))
      | "int" -> Some (Element.Text (Int_text, value))
      | "float" -> Some (Element.Text (Float_text, value))
      | _ -> None)
  | _ -> None

let mapper =
  object (self)
    inherit Scope.mapper as super

    method private frontend =
      {
        Element.name = "meltea";
        locals = "__melhole_dispatch" :: self#locals;
        map = self#expression;
        wrap = (fun loc child -> [%expr Html.node (fun __melhole_dispatch -> [%e child])]);
        unwrap =
          (fun loc element ->
            [%expr Html.render [%e element] __melhole_dispatch]);
        handler =
          (fun loc { label; requirement; value; _ } ->
            let send message =
              match label with
              | "onInput" | "onChange" ->
                  [%expr __melhole_dispatch ([%e message] (Html.target_value __melhole_event))]
              | "onCheck" ->
                  [%expr __melhole_dispatch ([%e message] (Html.target_checked __melhole_event))]
              | "onKeyDown" | "onKeyUp" | "onKeyPress" ->
                  [%expr __melhole_dispatch ([%e message] (Html.key __melhole_event))]
              | "onSubmit" ->
                  [%expr
                    Html.prevent_default __melhole_event;
                    __melhole_dispatch [%e message]]
              | _ -> [%expr __melhole_dispatch [%e message]]
            in
            match requirement with
            | `Required -> [%expr (fun __melhole_event -> [%e send value])]
            | `Optional ->
                [%expr
                  
                    (fun __melhole_event ->
                      Option.iter (fun __melhole_message -> [%e send [%expr __melhole_message]]) [%e value])]);
        style =
          (fun loc requirement value ->
            match requirement with
            | `Required -> [%expr ([%e value] : (string * string) list)]
            | `Optional ->
                [%expr
                  
                    (Option.value ~default:[] ([%e value] : (string * string) list option))]);
        reference =
          (fun loc _ _ ->
            Location.raise_errorf ~loc "meltea: views are pure; there are no refs in Meltea");
        classify = html_text;
        extra =
          (fun name ->
            match name with
            | "onCheck" -> Some (Dom_props.Handler ("change", "Form"))
            | _ -> None);
        each =
          (fun loc item ->
            match item.pexp_desc with
            | Pexp_apply
                ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "list"); _ }; _ },
                  [
                    ( Nolabel,
                      {
                        pexp_desc =
                          Pexp_apply
                            ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "List", "map"); _ }; _ },
                              [ (Nolabel, row); (Nolabel, items) ] );
                        _;
                      } );
                  ] ) ->
                Element.loop self#frontend ~loc ~definitions:self#definitions
                  ~view:(fun expression ->
                    match expression.pexp_desc with
                    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", _); _ }; _ }, _)
                    | Pexp_construct ({ txt = Lident ("::" | "[]"); _ }, _) ->
                        true
                    | _ -> false)
                  ~stable:[ [%expr __melhole_dispatch] ]
                  ~render:(fun view -> [%expr Html.render [%e view] __melhole_dispatch])
                  row
                  [%expr Array.of_list [%e self#expression items]]
            | _ -> None);
      }

    val mutable tracked : (string * string) list = []

    method! structure items =
      let known = tracked in
      let mapped =
        List.concat_map
          (fun item ->
            let companions =
              match item.pstr_desc with
              | Pstr_value (Nonrecursive, [ { pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; pvb_expr; _ } ]) ->
                  let loc = item.pstr_loc in
                  tracked <- List.filter (fun (_, other) -> not (String.equal other name)) tracked;
                  Option.to_list
                    (Option.map
                       (fun changes ->
                         tracked <- ("changes", name) :: tracked;
                         [%stri let [%p Ast_builder.Default.pvar ~loc ("__melhole_changes_" ^ name)] = [%e changes]])
                       (Fields.update ~loc pvb_expr))
                  @ Option.to_list
                      (Option.map
                         (fun reads ->
                           tracked <- ("reads", name) :: tracked;
                           [%stri
                             let [%p Ast_builder.Default.pvar ~loc ("__melhole_reads_" ^ name)] =
                               [%e Ast_builder.Default.eint ~loc reads]])
                         (Fields.view pvb_expr))
              | _ -> []
            in
            Hoist.structure self#structure_item [ item ] @ companions)
          items
      in
      tracked <- known;
      mapped

    method! expression expression =
      let loc = expression.pexp_loc in
      match expression.pexp_desc with
      | Pexp_apply
          ( ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Program", ("sandbox" | "element")); _ }; _ } as callee),
            arguments ) -> (
          let named label =
            List.find_map
              (fun (argument_label, argument) ->
                match (argument_label, argument.pexp_desc) with
                | Labelled found, Pexp_ident { txt = Lident name; _ }
                  when String.equal found label && not (List.mem name self#locals) ->
                    Some name
                | _ -> None)
              arguments
          in
          match (named "update", named "view") with
          | Some update, Some view
            when List.mem ("changes", update) tracked && List.mem ("reads", view) tracked ->
              super#expression
                {
                  expression with
                  pexp_desc =
                    Pexp_apply
                      ( callee,
                        ( Labelled "tracking",
                          [%expr
                            {
                              Program.changes = [%e Ast_builder.Default.evar ~loc ("__melhole_changes_" ^ update)];
                              reads = [%e Ast_builder.Default.evar ~loc ("__melhole_reads_" ^ view)];
                            }] )
                        :: arguments );
                }
          | _ -> super#expression expression)
      | Pexp_apply (callee, arguments) -> (
          match (Element.without_jsx expression.pexp_attributes, callee.pexp_desc) with
          | None, _ -> super#expression expression
          | Some _, Pexp_ident { txt = Lident tag; _ } ->
              Element.lowercase self#frontend ~loc tag arguments
          | Some _, _ ->
              Location.raise_errorf ~loc
                "meltea: views are functions; call (Module.view model) instead of <Module />")
      | Pexp_construct ({ txt = Lident ("[]" | "::"); _ }, _) -> (
          match Element.without_jsx expression.pexp_attributes with
          | Some _ ->
              Element.fragment self#frontend ~loc
                (Option.value ~default:[] (Element.list_items expression))
          | None -> super#expression expression)
      | _ -> super#expression expression
  end

let structure items = mapper#structure items
let () = Driver.register_transformation "meltea" ~impl:structure
