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

    method! structure items = Hoist.structure self#structure_item items

    method! expression expression =
      let loc = expression.pexp_loc in
      match expression.pexp_desc with
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
