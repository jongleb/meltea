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

    val mutable planned : Slice.t option = None

    method private frontend =
      let locals = "__melhole_dispatch" :: self#locals in
      {
        Element.name = "meltea";
        locals;
        gates = (fun loc -> Option.fold ~none:Element.Ungated ~some:(fun plan -> Slice.gates plan ~locals loc) planned);
        definitions = self#definitions;
        map = self#expression;
        wrap = (fun loc child -> [%expr Html.node (fun __melhole_dispatch -> [%e child])]);
        unwrap =
          (fun loc element ->
            [%expr Html.render [%e element] __melhole_dispatch]);
        handlers = Element.Linked "__melhole_dispatch";
        throwing = Element.Stops;
        handler =
          (fun loc { label; requirement; value; _ } ->
            let computed message =
              match label with
              | "onInput" | "onChange" -> [%expr [%e message] (Html.target_value __melhole_event)]
              | "onCheck" -> [%expr [%e message] (Html.target_checked __melhole_event)]
              | "onKeyDown" | "onKeyUp" | "onKeyPress" -> [%expr [%e message] (Html.key __melhole_event)]
              | _ -> message
            in
            let send message =
              match label with
              | "onSubmit" ->
                  [%expr
                    Html.prevent_default __melhole_event;
                    __melhole_dispatch [%e computed message]]
              | _ -> [%expr __melhole_dispatch [%e computed message]]
            in
            match (requirement, value.pexp_desc) with
            | `Required, _ ->
                let action wrapper =
                  match wrapper with
                  | "prevent" -> [%expr Html.prevent_default __melhole_event]
                  | _ -> [%expr Html.stop_propagation __melhole_event]
                in
                let rec peeled value =
                  match value.pexp_desc with
                  | Pexp_ident { txt = Ldot (Lident "Html", (("prevent" | "stop") as wrapper)); _ } -> action wrapper
                  | Pexp_apply
                      ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", (("prevent" | "stop") as wrapper)); _ }; _ },
                        [ (Nolabel, inner) ] ) ->
                      [%expr
                        [%e action wrapper];
                        [%e peeled inner]]
                  | Pexp_apply
                      ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "prevent_unless"); _ }; _ },
                        [
                          ( Nolabel,
                            { pexp_desc = Pexp_function ([], None, Pfunction_cases ([ { pc_lhs = inert; _ } ], _, _)); _ } );
                          (Nolabel, inner);
                        ] ) ->
                      [%expr
                        let __melhole_message = [%e computed inner] in
                        ((match __melhole_message with
                         | [%p inert] -> ()
                         | _ -> Html.prevent_default __melhole_event) [@ocaml.warning "-4-11-40-41-42"]);
                        __melhole_dispatch __melhole_message]
                  | _ -> send value
                in
                [%expr fun __melhole_event -> [%e peeled value]]
            | `Optional, _ ->
                [%expr
                  
                    (fun __melhole_event ->
                      Stdlib.Option.iter (fun __melhole_message -> [%e send [%expr __melhole_message]]) [%e value])]);
        style =
          (fun loc requirement value ->
            match requirement with
            | `Required -> [%expr ([%e value] : (string * string) list)]
            | `Optional ->
                [%expr
                  
                    (Stdlib.Option.value ~default:[] ([%e value] : (string * string) list option))]);
        reference =
          (fun loc _ _ ->
            Location.raise_errorf ~loc "meltea: views are pure; there are no refs in Meltea");
        inner_html = (fun loc value -> [%expr Melhole.View.html_of [%e value]]);
        ids = Element.No_ids;
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
                ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", (("list" | "array") as container)); _ }; _ },
                  [
                    ( Nolabel,
                      {
                        pexp_desc =
                          Pexp_apply
                            ( { pexp_desc = Pexp_ident { txt = Ldot ((Lident ("List" | "Array" as mapped) | Ldot (Lident "Stdlib", ("List" | "Array" as mapped))), "map"); _ }; _ },
                              [ (Nolabel, row); (Nolabel, items) ] );
                        _;
                      } );
                  ] ) ->
                Option.bind
                  (match (container, mapped) with
                  | "list", "List" -> Some [%expr Stdlib.Array.of_list [%e self#expression items]]
                  | "array", "Array" -> Some (self#expression items)
                  | _ -> None)
                @@ fun source ->
                Element.loop self#frontend ~loc ~definitions:self#definitions
                  ~view:(fun expression ->
                    match expression.pexp_desc with
                    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", _); _ }; _ }, _)
                    | Pexp_construct ({ txt = Lident ("::" | "[]"); _ }, _) ->
                        true
                    | _ -> false)
                  ~stable:[ [%expr __melhole_dispatch] ]
                  ~render:(fun view -> [%expr Html.render [%e view] __melhole_dispatch])
                  ~numbering:Element.Items row source
            | _ -> None
            in
            looped item);
      }

    val mutable tracked : (string * string) list = []

    method! structure items =
      let items = Fanout.annotate items in
      let shape =
        Digest.to_hex
          (Digest.string
             (Pprintast.string_of_structure
                (List.filter (fun item -> match item.pstr_desc with Pstr_type _ -> true | _ -> false) items)))
      in
      let known = tracked in
      let drawn =
        (object
           inherit [string list] Ast_traverse.fold as super

           method! expression expression found =
             match expression.pexp_desc with
             | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Program", ("sandbox" | "element")); _ }; _ }, arguments) ->
                 List.filter_map
                   (fun (label, argument) ->
                     match (label, argument.pexp_desc) with
                     | Labelled "view", Pexp_ident { txt = Lident name; _ } -> Some name
                     | _, _ -> None)
                   arguments
                 @ super#expression expression found
             | _ -> super#expression expression found
        end)
          #structure items [ "view" ]
      in
      let closers =
        let updates =
          (object
             inherit [string list] Ast_traverse.fold as super

             method! expression expression found =
               match expression.pexp_desc with
               | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Program", ("sandbox" | "element")); _ }; _ }, arguments) ->
                   List.filter_map
                     (fun (label, argument) ->
                       match (label, argument.pexp_desc) with
                       | Labelled "update", Pexp_ident { txt = Lident name; _ } -> Some name
                       | _, _ -> None)
                     arguments
                   @ super#expression expression found
               | _ -> super#expression expression found
          end)
            #structure items [ "update" ]
        in
        match
          List.filter_map
            (fun item ->
              match item.pstr_desc with
              | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; pvb_expr; _ } ]) when List.mem txt updates ->
                  Some (Fields.branches pvb_expr)
              | _ -> None)
            items
        with
        | [] -> None
        | found ->
            Option.map
              (List.concat_map (List.map (fun (branch : Fields.branch) -> Fields.mask (Fields.written branch.outcome))))
              (List.fold_right (fun branches all -> Option.bind branches (fun branches -> Option.map (fun all -> branches :: all) all)) found (Some []))
      in
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
                  @ (match (Fields.branches pvb_expr, name) with
                    | Some _, _ | None, "update" ->
                        tracked <- ("shape", name) :: tracked;
                        [
                          [%stri
                            let [%p Ast_builder.Default.pvar ~loc ("__melhole_shape_" ^ name)] =
                              Program.Shape [%e Ast_builder.Default.estring ~loc shape]];
                        ]
                    | None, _ -> [])
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
                              match (finder#expression pvb_expr false, name) with
                              | true, _ | false, "subscriptions" -> Some reads
                              | false, _ -> None)))
              | _ -> []
            in
            let sliced = ref [] in
            let compiled =
            Hoist.structure
              (fun item ->
                match (!Element.output, item.pstr_desc) with
                | Element.Server, _ -> self#structure_item item
                | (Element.Client_only | Element.Hydratable), Pstr_value (Nonrecursive, [ ({ pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; _ } as binding) ])
                  when List.mem name drawn && List.mem ("reads", name) tracked -> (
                    let item = Remember.structure_item item in
                    let binding = match item.pstr_desc with Pstr_value (_, [ binding ]) -> binding | _ -> binding in
                    planned <- Slice.plan ~closers binding;
                    let mapped = self#structure_item item in
                    let plan = planned in
                    planned <- None;
                    match (plan, mapped.pstr_desc) with
                    | Some plan, Pstr_value (flag, [ compiled ]) ->
                        let view, slice = Slice.split plan compiled in
                        Option.iter
                          (fun slice ->
                            let loc = item.pstr_loc in
                            tracked <- ("slice", name) :: tracked;
                            sliced :=
                              [
                                [%stri
                                  let [%p Ast_builder.Default.pvar ~loc ("__melhole_slice_" ^ name)] =
                                    Program.Sliced { analysed = [%e Ast_builder.Default.evar ~loc name]; draw = [%e slice] }];
                              ])
                          slice;
                        { mapped with pstr_desc = Pstr_value (flag, [ view ]) }
                    | (Some _ | None), _ -> mapped)
                | (Element.Client_only | Element.Hydratable), _ -> self#structure_item (Remember.structure_item item))
              [ item ]
            in
            compiled @ !sliced @ companions)
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
              let listening =
                Option.bind (named "subscriptions") (fun found -> Option.map (fun at -> (at, found)) (path found))
              in
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
                              ~reads:[%e companion "reads" [%expr Program.Unread] view_path]
                              ~shape:[%e companion "shape" [%expr Program.Unshaped] update_path]
                              ~slice:[%e companion "slice" [%expr Program.Unsliced] view_path]
                              ~subscriptions:
                                [%e
                                  Option.fold ~none:[%expr ()] ~some:(fun (_, subscriptions) -> subscriptions) listening]
                              ~listens:
                                [%e
                                  Option.fold ~none:[%expr Program.Unread]
                                    ~some:(fun (at, _) -> companion "reads" [%expr Program.Unread] at)
                                    listening]] )
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

let structure items =
  let items = Dead.annotate (Splice.structure items) in
  let inert = Dead.inert ~loc:Location.none items in
  let implied =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        let expression = super#expression expression in
        let loc = expression.pexp_loc in
        match (Element.without_jsx expression.pexp_attributes, expression.pexp_desc) with
        | Some _, Pexp_apply (({ pexp_desc = Pexp_ident { txt = Lident tag; _ }; _ } as callee), arguments) ->
            let given label = List.assoc_opt (Labelled label) arguments in
            let prevented value =
              match value.pexp_desc with
              | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "prevent"); _ }; _ }, _)
              | Pexp_ident { txt = Ldot (Lident "Html", "prevent"); _ } ->
                  value
              | _ ->
                  let loc = { value.pexp_loc with loc_ghost = true } in
                  { value with pexp_desc = Pexp_apply ([%expr Html.prevent], [ (Nolabel, value) ]) }
            in
            let nowhere =
              match (tag, Option.map (fun href -> href.pexp_desc) (given "href")) with
              | "a", Some (Pexp_constant (Pconst_string ("#", _, _))) -> [ "onClick" ]
              | _ -> []
            in
            let dropped = Option.fold ~none:[] ~some:(fun _ -> [ "onDrop"; "onDragEnter"; "onDragOver" ]) (given "onDrop") in
            let rec judged value =
              match (value.pexp_desc, inert) with
              | Pexp_apply (({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "stop"); _ }; _ } as stop), [ (Nolabel, inner) ]), _ ->
                  { value with pexp_desc = Pexp_apply (stop, [ (Nolabel, judged inner) ]) }
              | ( Pexp_apply
                    ( ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "prevent"); _ }; _ } as prevent),
                      [
                        ( Nolabel,
                          {
                            pexp_desc =
                              Pexp_apply
                                (({ pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "stop"); _ }; _ } as stop), [ (Nolabel, inner) ]);
                            _;
                          } );
                      ] ),
                  _ ) ->
                  judged
                    {
                      value with
                      pexp_desc =
                        Pexp_apply (stop, [ (Nolabel, { value with pexp_desc = Pexp_apply (prevent, [ (Nolabel, inner) ]) }) ]);
                    }
              | ( Pexp_apply
                    ( { pexp_desc = Pexp_ident { txt = Ldot (Lident "Html", "prevent"); _ }; _ },
                      [ (Nolabel, ({ pexp_desc = Pexp_function _ | Pexp_ident _ | Pexp_apply _ | Pexp_match _ | Pexp_ifthenelse _; _ } as inner)) ] ),
                  Some pattern ) ->
                  let loc = { value.pexp_loc with loc_ghost = true } in
                  {
                    value with
                    pexp_desc =
                      Pexp_apply ([%expr Html.prevent_unless], [ (Nolabel, [%expr function [%p pattern] -> ()]); (Nolabel, inner) ]);
                  }
              | _ -> value
            in
            {
              expression with
              pexp_desc =
                Pexp_apply
                  ( callee,
                    List.map
                      (fun (label, value) ->
                        match label with
                        | Labelled name when List.mem name (nowhere @ dropped) -> (label, judged (prevented value))
                        | Labelled name when String.length name > 2 && String.equal (String.sub name 0 2) "on" ->
                            (label, judged value)
                        | _ -> (label, value))
                      arguments
                    @ List.filter_map
                        (fun name ->
                          Option.fold
                            ~none:(Some (Labelled name, [%expr Html.prevent]))
                            ~some:(fun _ -> None) (given name))
                        dropped );
            }
        | _ -> expression
    end
  in
  let own =
    object
      inherit Ast_traverse.map as super

      method! longident longident =
        match longident with
        | Ldot (Ldot (Lident "Melhole", (("View" | "Served" | "Transition") as runtime)), name) ->
            Ldot (Ldot (Lident "Meltea_runtime", runtime), name)
        | Ldot (Lident "Melhole", (("View" | "Served" | "Transition") as runtime)) -> Ldot (Lident "Meltea_runtime", runtime)
        | _ -> longident

      method! expression expression =
        let expression = super#expression expression in
        let rec literal (value : expression) =
          let loc = value.pexp_loc in
          let cases = List.fold_right (fun case found -> Option.bind found (fun rest -> Option.map (fun pc_rhs -> { case with pc_rhs } :: rest) (literal case.pc_rhs))) in
          match value.pexp_desc with
          | Pexp_constant (Pconst_string ("", _, _)) -> Some [%expr None]
          | Pexp_constant (Pconst_string _) -> Some [%expr Some [%e value]]
          | Pexp_constraint (inner, _) -> literal inner
          | Pexp_match (scrutinee, given) -> Option.map (fun given -> { value with pexp_desc = Pexp_match (scrutinee, given) }) (cases given (Some []))
          | Pexp_ifthenelse (condition, yes, Some no) ->
              Option.bind (literal yes) (fun yes -> Option.map (fun no -> { value with pexp_desc = Pexp_ifthenelse (condition, yes, Some no) }) (literal no))
          | Pexp_let (flag, bindings, body) -> Option.map (fun body -> { value with pexp_desc = Pexp_let (flag, bindings, body) }) (literal body)
          | _ -> None
        in
        let classed (value : expression) =
          match value.pexp_desc with
          | Pexp_construct ({ txt = Lident "Some"; _ }, Some inner) -> Option.value ~default:value (literal inner)
          | _ -> value
        in
        let class_write (statement : expression) =
          match statement.pexp_desc with
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Ldot (Ldot (Lident "Meltea_runtime", "View"), ("put_attr" | "write_attr")); _ }; _ },
                arguments ) ->
              List.find_map (fun (_, (argument : expression)) -> match argument.pexp_desc with Pexp_constant (Pconst_string ("class", _, _)) -> Some arguments | _ -> None) arguments
          | _ -> None
        in
        match (expression.pexp_desc, !Element.output) with
        | Pexp_let (Nonrecursive, [ ({ pvb_pat = { ppat_desc = Ppat_var { txt = "__melhole_value"; _ }; _ }; _ } as binding) ], body), Element.Client_only -> (
            match (match body.pexp_desc with Pexp_sequence (first, _) -> class_write first | _ -> class_write body) with
            | Some _ -> { expression with pexp_desc = Pexp_let (Nonrecursive, [ { binding with pvb_expr = classed binding.pvb_expr } ], body) }
            | None -> expression)
        | Pexp_apply (callee, _), Element.Client_only ->
            Option.fold ~none:expression
              ~some:(fun arguments ->
                { expression with pexp_desc = Pexp_apply (callee, List.map (fun (label, argument) -> (label, classed argument)) arguments) })
              (class_write expression)
        | _ -> expression
    end
  in
  own#structure (mapper#structure (implied#structure items))
let () =
  Driver.add_arg "-hydratable"
    (Arg.Unit (fun () -> Element.output := Element.Hydratable))
    ~doc:" compile templates that can take over server-rendered HTML"

let () =
  Driver.add_arg "-server"
    (Arg.Unit (fun () -> Element.output := Element.Server))
    ~doc:" compile views to code that writes their HTML, for meltea.server, natively or under Melange"

let () = Driver.register_transformation "meltea" ~impl:structure
