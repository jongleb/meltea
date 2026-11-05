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
            | "onCheck" -> Some (Dom_props.Handler "change")
            | _ -> None);
        each =
          (fun loc item ->
            let rec looped item =
            match item.pexp_desc with
            | Pexp_apply
                ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", ("transition" | "transition_group" as kind)); _ }; _ },
                  arguments ) -> (
                match List.rev arguments with
                | (Nolabel, inner) :: settings ->
                    Option.map
                      (fun loop ->
                        Ast_builder.Default.pexp_apply ~loc
                          (Ast_builder.Default.evar ~loc (match kind with "transition" -> "Melhole.Transition.make" | _ -> "Melhole.Transition.group"))
                          (List.rev_map (fun (label, value) -> (label, self#expression value)) settings @ [ (Nolabel, loop) ]))
                      (looped inner)
                | _ -> None)
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
            | _ -> None
            in
            looped item);
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
                         [%stri
                           let [%p Ast_builder.Default.pvar ~loc ("__melhole_changes_" ^ name)] =
                             Program.Changes
                               { analysed = [%e Ast_builder.Default.evar ~loc name]; fields = [%e changes] }])
                       (Fields.update ~loc pvb_expr))
                  @ Option.to_list
                      (Option.map
                         (fun reads ->
                           tracked <- ("reads", name) :: tracked;
                           [%stri
                             let [%p Ast_builder.Default.pvar ~loc ("__melhole_reads_" ^ name)] =
                               Program.Reads
                                 {
                                   analysed = [%e Ast_builder.Default.evar ~loc name];
                                   fields = [%e Ast_builder.Default.eint ~loc reads];
                                 }])
                         (Option.bind (Fields.view pvb_expr) (fun reads ->
                              let finder =
                                object
                                  inherit [bool] Ast_traverse.fold as super

                                  method! expression expression found =
                                    found
                                    || Option.is_some (Element.without_jsx expression.pexp_attributes)
                                    || super#expression expression found
                                end
                              in
                              match finder#expression pvb_expr false with
                              | true -> Some reads
                              | false -> None)))
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
            arguments )
        when not (List.exists (fun (label, _) -> label = Labelled "tracking") arguments) -> (
          let named label =
            List.find_map
              (fun (argument_label, argument) ->
                match argument_label with
                | Labelled found when String.equal found label -> Some argument
                | _ -> None)
              arguments
          in
          let path expression =
            match expression.pexp_desc with
            | Pexp_ident { txt = Lident name; _ } when List.mem name self#locals -> None
            | Pexp_ident { txt; _ } -> Some txt
            | _ -> None
          in
          let analysed update =
            match update.pexp_desc with
            | Pexp_ident _ -> Option.map (fun found -> (found, update)) (path update)
            | Pexp_function
                ( [
                    { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = message; _ }; _ }); _ };
                    { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var { txt = model; _ }; _ }); _ };
                  ],
                  None,
                  Pfunction_body body ) -> (
                let calls expression =
                  match expression.pexp_desc with
                  | Pexp_apply
                      ( function_,
                        [
                          (Nolabel, { pexp_desc = Pexp_ident { txt = Lident first; _ }; _ });
                          (Nolabel, { pexp_desc = Pexp_ident { txt = Lident second; _ }; _ });
                        ] )
                    when String.equal first message && String.equal second model ->
                      Option.map (fun found -> (found, function_)) (path function_)
                  | _ -> None
                in
                match body.pexp_desc with
                | Pexp_apply _ -> calls body
                | Pexp_let (Nonrecursive, [ { pvb_pat; pvb_expr; _ } ], rest) -> (
                    match (pvb_pat.ppat_desc, rest.pexp_desc) with
                    | Ppat_var { txt = bound; _ }, Pexp_ident { txt = Lident result; _ }
                    | ( Ppat_tuple ({ ppat_desc = Ppat_var { txt = bound; _ }; _ } :: _),
                        Pexp_tuple ({ pexp_desc = Pexp_ident { txt = Lident result; _ }; _ } :: _) )
                      when String.equal bound result ->
                        calls pvb_expr
                    | _ -> None)
                | _ -> None)
            | _ -> None
          in
          let companion kind fallback found =
            let name = "__melhole_" ^ kind ^ "_" ^ Longident.last_exn found in
            match found with
            | Lident short ->
                if List.mem (kind, short) tracked then Ast_builder.Default.evar ~loc name else fallback
            | Ldot (owner, _) ->
                Ast_builder.Default.pexp_open ~loc
                  (Ast_builder.Default.open_infos ~loc ~override:Fresh
                     ~expr:
                       (Ast_builder.Default.pmod_structure ~loc
                          [ [%stri let [%p Ast_builder.Default.pvar ~loc name] = [%e fallback]] ]))
                  (Ast_builder.Default.pexp_open ~loc
                     {
                       (Ast_builder.Default.open_infos ~loc ~override:Override
                          ~expr:(Ast_builder.Default.pmod_ident ~loc { txt = owner; loc }))
                       with
                       popen_attributes =
                         [
                           Ast_builder.Default.attribute ~loc ~name:{ txt = "ocaml.warning"; loc }
                             ~payload:(PStr [ [%stri "-33-44-45"] ]);
                         ];
                     }
                     (Ast_builder.Default.evar ~loc name))
            | Lapply _ -> fallback
          in
          match
            ( Option.bind (named "update") analysed,
              Option.bind (named "view") (fun view -> Option.map (fun found -> (found, view)) (path view)) )
          with
          | Some (update_path, update), Some (view_path, view) ->
              super#expression
                {
                  expression with
                  pexp_desc =
                    Pexp_apply
                      ( callee,
                        ( Labelled "tracking",
                          [%expr
                            Program.track ~update:[%e update]
                              ~changes:[%e companion "changes" [%expr Program.Unanalysed] update_path]
                              ~view:[%e view]
                              ~reads:[%e companion "reads" [%expr Program.Unread] view_path]] )
                        :: arguments );
                }
          | _ -> super#expression expression)
      | Pexp_apply (callee, arguments) -> (
          match (Element.without_jsx expression.pexp_attributes, callee.pexp_desc) with
          | None, _ -> super#expression expression
          | Some attributes, Pexp_ident { txt = Lident tag; _ } ->
              Element.lowercase self#frontend ~loc ~attributes tag arguments
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
