open Ppxlib
open Ast_builder.Default

type text = String_text | Int_text | Float_text
type special = Text of text * expression | Nothing

type handler = {
  label : string;
  event : string;
  requirement : [ `Required | `Optional ];
  value : expression;
}

type handlers = Closures | Linked of string

(** What a front end does after a render throws halfway through a template. *)
type throwing = Recovers | Stops
type ids = Tree_ids | No_ids
type output = Client_only | Hydratable | Server

let output = ref Client_only

type frontend = {
  name : string;
  locals : string list;
  map : expression -> expression;
  wrap : location -> expression -> expression;
  unwrap : location -> expression -> expression;
  handler : location -> handler -> expression;
  handlers : handlers;
  throwing : throwing;
  style : location -> [ `Required | `Optional ] -> expression -> expression;
  reference : location -> [ `Required | `Optional ] -> expression -> expression;
  inner_html : location -> expression -> expression;
  ids : ids;
  classify : expression -> special option;
  extra : string -> Dom_props.kind option;
  each : location -> expression -> expression option;
}

(** What one DOM prop becomes. *)
type prop =
  | Static of string * Markup.static  (** Written into the template. *)
  | Dynamic of Markup.site * expression  (** A hole and its value. *)
  | Absent

let float_text ~loc =
  match !output with
  | Server -> [%expr Melhole_server.float]
  | Client_only | Hydratable -> [%expr Js.Float.toString]

let without_jsx attributes =
  match
    List.partition (fun attribute -> String.equal attribute.attr_name.txt "JSX") attributes
  with
  | [], _ -> None
  | _ :: _, rest -> Some rest

let rec list_items expression =
  match expression.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> Some []
  | Pexp_construct
      ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ head; tail ]; _ }) ->
      Option.map (fun rest -> head :: rest) (list_items tail)
  | _ -> None

let lowercase_call expression =
  match expression.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident tag; _ }; _ }, arguments) ->
      Option.map (fun _ -> (tag, arguments)) (without_jsx expression.pexp_attributes)
  | _ -> None

let prop frontend ~loc ~label kind requirement value =
  let hole site expression = Dynamic (site, expression) in
  let kind : Dom_props.kind =
    match (!output, kind) with
    | Server, Dom_props.Attribute (name, value) -> Dom_props.Attribute (Dom_props.react_name name, value)
    | (Server | Client_only | Hydratable), kind -> kind
  in
  let literal =
    match value.pexp_desc with
    | Pexp_constant (Pconst_string (text, _, _)) -> `String text
    | Pexp_constant (Pconst_integer (digits, None)) -> `Number digits
    | Pexp_constant (Pconst_float (digits, None)) -> `Number digits
    | Pexp_construct ({ txt = Lident "true"; _ }, None) -> `True
    | Pexp_construct ({ txt = Lident "false"; _ }, None) -> `False
    | _ -> `Dynamic
  in
  let type_of (value : Dom_props.value) =
    match value with
    | Text -> [%type: string]
    | Flag | Booleanish -> [%type: bool]
    | Integer -> [%type: int]
    | Decimal -> [%type: float]
  in
  let property site kind requirement =
    match requirement with
    | `Required -> hole site [%expr ([%e value] : [%t type_of kind])]
    | `Optional ->
        let default =
          match kind with
          | Dom_props.Text -> [%expr ""]
          | Flag | Booleanish -> [%expr false]
          | Integer -> [%expr 0]
          | Decimal -> [%expr 0.]
        in
        hole site
          [%expr (Stdlib.Option.value ~default:[%e default] ([%e value] : [%t type_of kind] option))]
  in
  let bound =
    match (!output, kind) with
    | Server, Attribute (name, Integer) -> Dom_props.positive name
    | (Server | Client_only | Hydratable), _ -> `Any
  in
  match (kind, requirement, literal, bound) with
  | Attribute (_, Integer), `Required, `Number digits, `Positive when int_of_string digits < 1 -> Absent
  | Attribute (name, Integer), `Required, (`String _ | `True | `False | `Dynamic), `Positive ->
      hole (Attr name)
        [%expr
          (match ([%e value] : int) with
          | __melhole_number when __melhole_number >= 1 -> Some (string_of_int __melhole_number)
          | _ -> None)]
  | Attribute (name, Integer), `Optional, _, `Positive ->
      hole (Attr name)
        [%expr
          Stdlib.Option.bind ([%e value] : int option) (fun __melhole_number ->
              match __melhole_number >= 1 with true -> Some (string_of_int __melhole_number) | false -> None)]
  | Dom_props.Ignored, _, _, _ -> Absent
  | Attribute (name, Text), `Required, `String text, _ -> Static (name, Valued text)
  | Attribute (name, Text), `Required, _, _ ->
      hole (Attr name) [%expr (Some ([%e value] : string))]
  | Attribute (name, Flag), `Required, `True, _ -> Static (name, Bare)
  | Attribute (_, Flag), `Required, `False, _ -> Absent
  | Attribute (name, Flag), `Required, _, _ ->
      hole (Attr name)
        [%expr (if ([%e value] : bool) then Some "" else None)]
  | Attribute (name, Booleanish), `Required, `True, _ -> Static (name, Valued "true")
  | Attribute (name, Booleanish), `Required, `False, _ -> Static (name, Valued "false")
  | Attribute (name, Booleanish), `Required, _, _ ->
      hole (Attr name)
        [%expr
          (Some (if ([%e value] : bool) then "true" else "false"))]
  | Attribute (name, (Integer | Decimal)), `Required, `Number digits, _ ->
      Static (name, Valued digits)
  | Attribute (name, Integer), `Required, _, _ ->
      hole (Attr name)
        [%expr (Some (string_of_int ([%e value] : int)))]
  | Attribute (name, Decimal), `Required, _, _ ->
      hole (Attr name)
        [%expr (Some ([%e float_text ~loc] ([%e value] : float)))]
  | Attribute (name, Text), `Optional, _, _ ->
      hole (Attr name) [%expr ([%e value] : string option)]
  | Attribute (name, Flag), `Optional, _, _ ->
      hole (Attr name)
        [%expr
          
            (match ([%e value] : bool option) with
            | Some true -> Some ""
            | Some false | None -> None)]
  | Attribute (name, Booleanish), `Optional, _, _ ->
      hole (Attr name)
        [%expr
          
            (Stdlib.Option.map
               (fun flag -> if flag then "true" else "false")
               ([%e value] : bool option))]
  | Attribute (name, Integer), `Optional, _, _ ->
      hole (Attr name)
        [%expr (Stdlib.Option.map string_of_int ([%e value] : int option))]
  | Attribute (name, Decimal), `Optional, _, _ ->
      hole (Attr name)
        [%expr
          (Stdlib.Option.map [%e float_text ~loc] ([%e value] : float option))]
  | Property (name, kind), requirement, _, _ -> property (Prop name) kind requirement
  | Controlled (name, kind), requirement, _, _ -> property (Control name) kind requirement
  | Handler event, requirement, _, _ ->
      hole (Handler event) (frontend.handler loc { label; event; requirement; value })
  | Style, requirement, _, _ -> hole Style (frontend.style loc requirement value)
  | Ref, requirement, _, _ -> hole Ref (frontend.reference loc requirement value)
  | Inner_html, `Required, _, _ -> hole (Prop "innerHTML") (frontend.inner_html loc value)
  | Inner_html, `Optional, _, _ ->
      hole (Prop "innerHTML")
        [%expr Stdlib.Option.fold ~none:"" ~some:(fun __melhole_html -> [%e frontend.inner_html loc [%expr __melhole_html]]) [%e value]]

let fork_marker = "melhole.forks"

let forked frontend ~loc forks expression =
  match (frontend.ids, forks, !output) with
  | No_ids, _, _ | Tree_ids, [], _ | Tree_ids, _, Client_only -> expression
  | Tree_ids, _ :: _, (Server | Hydratable) ->
      {
        expression with
        pexp_attributes =
          attribute ~loc ~name:{ txt = fork_marker; loc }
            ~payload:
              (PStr
                 [
                   pstr_eval ~loc
                     (pexp_array ~loc (List.concat_map (fun (count, index) -> [ eint ~loc count; eint ~loc index ]) forks))
                     [];
                 ])
          :: expression.pexp_attributes;
      }

let fork_path expression =
  List.find_map
    (fun attribute ->
      match (String.equal attribute.attr_name.txt fork_marker, attribute.attr_payload) with
      | true, PStr [ { pstr_desc = Pstr_eval (path, _); _ } ] -> Some path
      | _ -> None)
    expression.pexp_attributes

let rec analyse frontend ~loc ~forks tag arguments =
  let given = ref [] and children = ref [] and key = ref None in
  let ranks = List.mapi (fun index prop -> (Dom_props.Prop.to_string prop, index)) Dom_props.Prop.all in
  List.iter
    (fun (label, value) ->
      match label with
      | Nolabel -> ()
      | Labelled "key" -> key := Some (frontend.map value)
      | Labelled "children" ->
          children :=
            (match list_items value with
            | Some items ->
                let count = List.length items in
                List.concat
                  (List.mapi
                     (fun index item ->
                       Option.to_list
                         (child frontend
                            ~forks:(match count with 0 | 1 -> forks | _ -> forks @ [ (count, index) ])
                            item))
                     items)
            | None ->
                [
                  Markup.Slot
                    ( Child_slot,
                      forked frontend ~loc forks [%expr [%e frontend.unwrap loc (frontend.map value)]] );
                ])
      | Labelled name | Optional name -> (
          let requirement =
            match label with Optional _ -> `Optional | Labelled _ | Nolabel -> `Required
          in
          match
            match (Dom_props.Prop.of_string name, tag) with
            | Some _, "option" when String.equal name "value" -> Some (Dom_props.Attribute ("value", Text))
            | Some _, "textarea" when String.equal name "defaultValue" && not (!output = Server) ->
                Some (Dom_props.Property ("defaultValue", Text))
            | Some parsed, _ -> Some (Dom_props.kind parsed)
            | None, _ -> frontend.extra name
          with
          | None ->
              Location.raise_errorf ~loc:value.pexp_loc "%s: <%s> has no prop %s" frontend.name tag
                name
          | Some kind -> (
              match (!output, name, tag) with
              | Server, ("defaultValue" | "defaultChecked"), tag
                when not (List.mem tag [ "input"; "textarea"; "select" ]) ->
                  ()
              | (Server | Client_only | Hydratable), _, _ ->
                  given :=
                    ( ( (match requirement with `Required -> 0 | `Optional -> 1),
                        Option.value ~default:(List.length ranks) (List.assoc_opt name ranks) ),
                      prop frontend ~loc ~label:name kind requirement (frontend.map value) )
                    :: !given)))
    arguments;
  (match (Markup.void tag, !children) with
  | `Void, _ :: _ -> Location.raise_errorf ~loc "%s: <%s> cannot have children" frontend.name tag
  | `Void, [] | `Container, _ -> ());
  let ordered =
    match !output with
    | Server ->
        List.map
          (fun (_, made) ->
            match made with
            | Static (attribute, Valued text) -> Dynamic (Attr attribute, [%expr Some [%e estring ~loc text]])
            | Static (attribute, Bare) -> Dynamic (Attr attribute, [%expr Some ""])
            | Dynamic _ | Absent -> made)
          (List.stable_sort (fun (first, _) (second, _) -> compare first second) (List.rev !given))
    | Client_only | Hydratable -> List.rev_map snd !given
  in
  ( {
      Markup.tag;
      statics = List.filter_map (fun made -> match made with Static (attribute, static) -> Some (attribute, static) | Dynamic _ | Absent -> None) ordered;
      dynamics = List.filter_map (fun made -> match made with Dynamic (site, hole) -> Some (site, hole) | Static _ | Absent -> None) ordered;
      children = !children;
    },
    !key )

and child frontend ~forks item =
  let loc = item.pexp_loc in
  match lowercase_call item with
  | Some (tag, arguments) -> Some (Markup.Element (fst (analyse frontend ~loc ~forks tag arguments)))
  | None -> (
      match frontend.classify item with
      | Some Nothing -> None
      | Some (Text (String_text, { pexp_desc = Pexp_constant (Pconst_string (text, _, _)); _ })) ->
          Some (Markup.Text text)
      | Some (Text (Int_text, { pexp_desc = Pexp_constant (Pconst_integer (digits, None)); _ })) ->
          Some (Markup.Text (string_of_int (int_of_string digits)))
      | Some (Text (Float_text, { pexp_desc = Pexp_constant (Pconst_float (digits, None)); _ })) ->
          Some (Markup.Text (Melhole_server.float (float_of_string digits)))
      | Some (Text (Int_text, value)) ->
          Some
            (Markup.Slot
               ( Text_slot,
                 [%expr (string_of_int ([%e frontend.map value] : int))] ))
      | Some (Text (Float_text, value)) ->
          Some
            (Markup.Slot
               ( Text_slot,
                 [%expr
                   ([%e float_text ~loc] ([%e frontend.map value] : float))] ))
      | Some (Text (String_text, value)) ->
          Some
            (Markup.Slot
               (Text_slot, [%expr ([%e frontend.map value] : string)]))
      | None ->
          Some
            (Markup.Slot
               ( Child_slot,
                 forked frontend ~loc forks
                   (match frontend.each loc item with
                   | Some loop -> loop
                   | None -> frontend.unwrap loc (frontend.map item)) )))

let row_marker = "melhole.row"

let served frontend ~loc (element : expression Markup.element) key =
  let writer = [%expr __melhole_writer] in
  let checks = ref [] and bindings = ref [] and choices = ref 0 in
  let bound value =
    let name = "__melhole_bound_" ^ string_of_int (List.length !bindings) in
    bindings := (name, value) :: !bindings;
    evar ~loc name
  in
  let rec ops (tree : expression Markup.element) =
    let preformatted = match tree.tag with "pre" | "listing" | "textarea" -> `Preformatted | _ -> `Plain in
    let written name value =
      match value with
      | [%expr Some [%e? { pexp_desc = Pexp_constant (Pconst_string (text, _, _)); _ }]] ->
          `Tag (" " ^ name ^ "=\"" ^ Melhole_server.escape text ^ "\"")
      | _ -> `Run [%expr Melhole_server.attribute [%e writer] [%e estring ~loc name] [%e value]]
    in
    let preferred group =
      match List.partition (fun ((site : Markup.site), _) -> match site with Attr _ -> false | _ -> true) group with
      | given :: _, _ | [], given :: _ -> Some given
      | [], [] -> None
    in
    let named name ((site : Markup.site), _) =
      match site with
      | Attr found | Prop found | Control found -> String.equal found name
      | Handler _ | Ref | Style | Sole_text -> false
    in
    let values, rest =
      match tree.tag with
      | "input" | "textarea" | "select" -> List.partition (named "value") tree.dynamics
      | _ -> ([], tree.dynamics)
    in
    let checks_of, rest =
      match tree.tag with "input" -> List.partition (named "checked") rest | _ -> ([], rest)
    in
    let marked, rest =
      match tree.tag with
      | "option" -> List.partition (fun ((site : Markup.site), _) -> match site with Prop "selected" -> true | _ -> false) rest
      | _ -> ([], rest)
    in
    let content = ref None and choice = ref None in
    let attribute ((site : Markup.site), value) =
      match site with
      | Attr name -> Some (written name value)
      | Prop "innerHTML" ->
          let html = bound value in
          content :=
            Some
              (match preformatted with
              | `Preformatted -> [%expr Melhole_server.leading_newline [%e writer] [%e html]; Melhole_server.raw [%e writer] [%e html]]
              | `Plain -> [%expr Melhole_server.raw [%e writer] [%e html]]);
          None
      | Prop "value" | Control "value" -> Some (`Run [%expr Melhole_server.attribute [%e writer] "value" (Some [%e value])])
      | Prop name | Control name -> Some (`Run [%expr Melhole_server.flag [%e writer] [%e estring ~loc name] [%e value]])
      | Style -> Some (`Run [%expr Melhole_server.style [%e writer] [%e value]])
      | Handler _ | Ref | Sole_text ->
          checks := [%expr ignore (fun () -> [%e value])] :: !checks;
          None
    in
    let rest, option_value =
      match (tree.tag, List.find_opt (named "value") rest) with
      | "option", Some ((site : Markup.site), value) ->
          let shared = bound value in
          ( List.map (fun (found, given) -> match given == value with true -> (found, shared) | false -> (found, given)) rest,
            Some (match site with Attr _ -> [%expr Stdlib.Option.value ~default:"" [%e shared]] | _ -> shared) )
      | "option", None ->
          ( rest,
            Some
              (List.fold_left
                 (fun text (node : expression Markup.node) ->
                   match node with
                   | Text static -> [%expr [%e text] ^ [%e estring ~loc static]]
                   | Slot (Text_slot, value) -> [%expr [%e text] ^ [%e value]]
                   | Slot (Child_slot, _) | Element _ -> text)
                 [%expr ""] tree.children) )
      | _ -> (rest, None)
    in
    let attributes = List.filter_map attribute rest in
    let tail_attributes =
      Option.to_list
        (Option.map
           (fun ((site : Markup.site), value) ->
             match site with
             | Attr _ -> `Run [%expr Melhole_server.attribute [%e writer] "checked" [%e value]]
             | _ -> `Run [%expr Melhole_server.flag [%e writer] "checked" [%e value]])
           (preferred checks_of))
      @ (match tree.tag with
        | "input" ->
            Option.to_list
              (Option.map
                 (fun ((site : Markup.site), value) ->
                   match site with
                   | Attr _ -> `Run [%expr Melhole_server.attribute [%e writer] "value" [%e value]]
                   | _ -> `Run [%expr Melhole_server.attribute [%e writer] "value" (Some [%e value])])
                 (preferred values))
        | "textarea" ->
            Option.iter
              (fun ((site : Markup.site), value) ->
                let text =
                  bound (match site with Attr _ -> [%expr Stdlib.Option.value ~default:"" [%e value]] | _ -> value)
                in
                content :=
                  Some
                    [%expr
                      Melhole_server.leading_newline [%e writer] [%e text];
                      Melhole_server.dynamic_text [%e writer] [%e text]])
              (preferred values);
            []
        | "select" ->
            choice :=
              Option.map
                (fun ((site : Markup.site), value) -> match site with Attr _ -> value | _ -> [%expr Some [%e value]])
                (preferred values);
            []
        | _ -> [])
      @ Option.to_list
          (Option.map
             (fun value ->
               `Run
                 [%expr
                   Melhole_server.option_selected [%e writer] [%e value]
                     [%e
                       Option.fold ~none:[%expr Melhole_server.Unmarked]
                         ~some:(fun (_, flag) ->
                           [%expr match [%e flag] with true -> Melhole_server.Marked | false -> Melhole_server.Unmarked])
                         (List.nth_opt marked 0)]])
             option_value)
    in
    let closing = match Markup.void tree.tag with `Void -> "/>" | `Container -> ">" in
    let rec children nodes known =
      match nodes with
      | [] -> ([], known)
      | node :: rest ->
          let now, after =
            match ((node : expression Markup.node), known) with
            | Text "", _ -> ([], known)
            | Text text, `Other -> ([ `Text (Melhole_server.escape text) ], `Text)
            | Text text, `Text -> ([ `Text ("<!-- -->" ^ Melhole_server.escape text) ], `Text)
            | Text text, `Unknown ->
                ([ `Run [%expr Melhole_server.static_text [%e writer] [%e estring ~loc (Melhole_server.escape text)]] ], `Text)
            | Element child, _ -> (ops child, `Other)
            | Slot (Text_slot, value), _ -> ([ `Run [%expr Melhole_server.dynamic_text [%e writer] [%e value]] ], `Unknown)
            | Slot (Child_slot, value), _ ->
                ( [
                    `Run
                      (Option.fold
                         ~none:[%expr Melhole_server.write [%e writer] [%e value]]
                         ~some:(fun path ->
                           [%expr
                             Melhole_server.forked [%e writer] [%e path]
                               [%e
                                 {
                                   value with
                                   pexp_attributes =
                                     List.filter
                                       (fun attribute -> not (String.equal attribute.attr_name.txt fork_marker))
                                       value.pexp_attributes;
                                 }]])
                         (fork_path value));
                  ],
                  `Unknown )
          in
          let later, last = children rest after in
          (now @ later, last)
    in
    let nodes =
      match (preformatted, tree.tag, tree.children) with
      | `Preformatted, ("pre" | "listing"), [ Text text ] when String.length text > 0 && text.[0] = '\n' -> [ Markup.Text ("\n" ^ text) ]
      | `Preformatted, ("pre" | "listing"), [ Slot (Text_slot, value) ] ->
          let text = bound value in
          content :=
            Some
              [%expr
                Melhole_server.leading_newline [%e writer] [%e text];
                Melhole_server.dynamic_text [%e writer] [%e text]];
          []
      | _, _, nodes -> nodes
    in
    let inner = Option.fold ~none:(fst (children nodes `Other)) ~some:(fun run -> [ `Run run ]) !content in
    let inner =
      Option.fold ~none:inner
        ~some:(fun value ->
          incr choices;
          let around = "__melhole_choice_" ^ string_of_int !choices in
          (`Choose (around, value) :: inner) @ [ `Run [%expr Melhole_server.chosen [%e writer] [%e evar ~loc around]] ])
        !choice
    in
    let tail = match Markup.void tree.tag with `Void -> [] | `Container -> [ `Tag ("</" ^ tree.tag ^ ">") ] in
    (`Tag ("<" ^ tree.tag) :: attributes) @ tail_attributes @ (`Tag closing :: inner) @ tail
  in
  let steps = ops element in
  let rec merged steps =
    match steps with
    | `Tag first :: `Tag second :: rest -> merged (`Tag (first ^ second) :: rest)
    | `Tag first :: `Text second :: rest -> merged (`Text (first ^ second) :: rest)
    | `Text first :: `Text second :: rest -> merged (`Text (first ^ second) :: rest)
    | `Text first :: `Tag second :: rest -> merged (`Tag (first ^ second) :: rest)
    | step :: rest -> step :: merged rest
    | [] -> []
  in
  let body =
    List.fold_right
      (fun step rest ->
        let run =
          match step with
          | `Tag chunk -> [%expr Melhole_server.markup [%e writer] [%e estring ~loc chunk]]
          | `Text chunk -> [%expr Melhole_server.markup_text [%e writer] [%e estring ~loc chunk]]
          | `Run run -> run
          | `Choose (_, value) -> [%expr Melhole_server.choose [%e writer] [%e value]]
        in
        match step with
        | `Choose (around, _) -> [%expr let [%p pvar ~loc around] = [%e run] in [%e rest]]
        | `Tag _ | `Text _ | `Run _ -> [%expr [%e run]; [%e rest]])
      (List.map (fun run -> `Run run) (List.rev !checks) @ merged steps)
      [%expr ()]
  in
  let body =
    List.fold_left
      (fun body (name, value) -> [%expr let [%p pvar ~loc name] = [%e value] in [%e body]])
      body !bindings
  in
  let instance = frontend.wrap loc [%expr Melhole_server.element (fun __melhole_writer -> [%e body])] in
  Option.fold ~none:instance
    ~some:(fun key -> [%expr ignore ([%e key] : string); [%e instance]])
    key

let lowercase frontend ~loc ~attributes tag arguments =
  let element, key = analyse frontend ~loc ~forks:[] tag arguments in
  match !output with
  | Server -> served frontend ~loc element key
  | Client_only | Hydratable ->
  let role =
    match (List.find_opt (fun attribute -> String.equal attribute.attr_name.txt row_marker) attributes, key) with
    | Some _, Some key -> `Row key
    | Some _, None | None, _ -> `Child
  in
  let html, holes = Markup.render element in
  let name indices =
    "__melhole_node" ^ String.concat "" (List.map (fun index -> "_" ^ string_of_int index) indices)
  in
  let node indices = evar ~loc (name indices) in
  let walk = Markup.walk holes in
  let position indices index =
    let before =
      match Markup.anchor indices index with
      | First_or_end parent -> [%expr Melhole.Template.first_or_end [%e node parent]]
      | Next_or_end previous -> [%expr Melhole.Template.next_or_end [%e node previous]]
    in
    [%expr { Melhole.Template.parent = [%e node indices]; before = [%e before] }]
  in
  let at indices = [%expr Melhole.Template.element [%e node indices]] in
  let hole_name index = "__melhole_hole_" ^ string_of_int index in
  let key_name index = "__melhole_key_" ^ string_of_int index in
  let made_holes =
    List.mapi
      (fun index (hole : expression Markup.hole) ->
        let one creator value = ([ (hole_name index, creator) ], value) in
        match hole with
        | On_element (indices, (Attr _ | Sole_text), value) -> one (at indices) value
        | On_element (indices, Handler event, value) ->
            ( [
                (hole_name index, at indices);
                (key_name index, [%expr Melhole.Events.listen [%e at indices] [%e estring ~loc event]]);
              ],
              value )
        | At_position (indices, index_in, Text_slot, value) ->
            one [%expr Melhole.View.text_at [%e position indices index_in]] value
        | On_element (indices, Prop name, value) ->
            one [%expr Melhole.View.prop_hole [%e at indices] [%e estring ~loc name]] value
        | On_element (indices, Control name, value) ->
            one [%expr Melhole.View.controlled_hole [%e at indices] [%e estring ~loc name]] value
        | On_element (indices, Ref, value) -> one [%expr Melhole.View.reference_hole [%e at indices]] value
        | On_element (indices, Style, value) -> one [%expr Melhole.View.style_hole [%e at indices]] value
        | At_position (indices, index_in, Child_slot, value) ->
            one [%expr Melhole.View.content_hole [%e position indices index_in]] value)
      holes
  in
  let root_name = "__melhole_root" in
  let creators, same, effects, keys =
    let kept, same, effects, keys =
      List.fold_left
        (fun (kept, same, effects, keys) (named, creator) ->
          match creator with
          | [%expr Melhole.Events.listen [%e? element] [%e? event]] ->
              ( kept,
                same,
                [%expr Melhole.Events.listen [%e element] [%e event]] :: effects,
                (named, Hoist.hoist ~loc "event" [%expr Melhole.View.event_key [%e event]]) :: keys )
          | [%expr Melhole.Template.element [%e? _]] -> (
              let printed = Pprintast.string_of_expression creator in
              match
                List.find_opt (fun (_, other) -> String.equal (Pprintast.string_of_expression other) printed) kept
              with
              | Some (original, _) -> (kept, (named, original) :: same, effects, keys)
              | None -> ((named, creator) :: kept, same, effects, keys))
          | _ -> ((named, creator) :: kept, same, effects, keys))
        ([], [], [], [])
        ((match role with `Row _ -> [ (root_name, at []) ] | `Child -> []) @ List.concat_map fst made_holes)
    in
    (List.rev kept, List.rev same, List.rev effects, List.rev keys)
  in
  let original named = Option.value ~default:named (List.assoc_opt named same) in
  let effected body = List.fold_right (fun registered body -> [%expr [%e registered]; [%e body]]) effects body in
  let bound body =
    List.fold_right
      (fun (named, value) body -> [%expr let [%p pvar ~loc named] = [%e value] in [%e body]])
      (List.map (fun (named, original) -> (named, evar ~loc original)) same @ keys)
      body
  in
  let values = List.map snd made_holes in
  let walked body =
    List.fold_right
      (fun (indices, (step : Markup.step)) body ->
        let reach =
          match step with
          | First_of parent -> [%expr Melhole.Template.first [%e node parent]]
          | Next_of previous -> [%expr Melhole.Template.next [%e node previous]]
        in
        [%expr let [%p pvar ~loc (name indices)] = [%e reach] in [%e body]])
      walk body
  in
  let markup =
    match element.tag with
    | "circle" | "clipPath" | "defs" | "ellipse" | "feBlend" | "feColorMatrix" | "feComposite"
    | "feFlood" | "feGaussianBlur" | "feMerge" | "feMergeNode" | "feOffset" | "filter"
    | "foreignObject" | "g" | "image" | "line" | "linearGradient" | "marker" | "mask" | "path"
    | "pattern" | "polygon" | "polyline" | "radialGradient" | "rect" | "stop" | "symbol"
    | "text" | "textPath" | "tspan" | "use" ->
        [%expr Melhole.Template.svg [%e estring ~loc html]]
    | _ -> [%expr Melhole.Template.html [%e estring ~loc html]]
  in
  let guards =
    List.map
      (fun (hole : expression Markup.hole) ->
        match hole with
        | On_element (_, Ref, _) -> Free.Volatile
        | On_element (_, (Attr _ | Prop _ | Control _ | Style | Handler _ | Sole_text), value)
        | At_position (_, _, _, value) ->
            match Free.of_expression value with
            | Free.Reads names ->
                Free.Reads (List.filter (fun name -> List.mem name frontend.locals) names)
            | Free.Volatile -> Free.Volatile)
      holes
  in
  let variables =
    List.sort_uniq String.compare
      (List.concat_map (fun guard -> match guard with Free.Reads names -> names | Free.Volatile -> []) guards)
  in
  let constant = List.length variables in
  let mounted =
    List.find_map
      (fun guard -> match guard with Free.Reads [] -> Some constant | Free.Reads (_ :: _) | Free.Volatile -> None)
      guards
  in
  let guard_slots = constant + Option.fold ~none:0 ~some:(fun _ -> 1) mounted in
  let value_slots =
    let rec injective expression =
      match expression.pexp_desc with
      | Pexp_ident { txt = Lident read; _ } -> Some read
      | Pexp_constraint (inner, _) | Pexp_construct ({ txt = Lident "Some"; _ }, Some inner) -> injective inner
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "string_of_int" | Ldot (Lident "Stdlib", "string_of_int"); _ }; _ }, [ (Nolabel, inner) ]) ->
          injective inner
      | _ -> None
    in
    List.rev
      (snd
         (List.fold_left
            (fun (next, found) (index, guard, (hole : expression Markup.hole)) ->
              match (hole, guard) with
              | (On_element (_, (Attr _ | Sole_text), value) | At_position (_, _, Text_slot, value)), Free.Reads [ read ]
                when Option.equal String.equal (injective value) (Some read) ->
                  (next, found)
              | (On_element (_, (Attr _ | Sole_text), _) | At_position (_, _, Text_slot, _)), Free.Reads [] -> (next, found)
              | (On_element (_, (Attr _ | Sole_text), _) | At_position (_, _, Text_slot, _)), (Free.Reads (_ :: _) | Free.Volatile) ->
                  (next + 1, (index, next) :: found)
              | (On_element (_, (Prop _ | Control _ | Handler _ | Ref | Style), _) | At_position (_, _, Child_slot, _)), _ ->
                  (next, found))
            (guard_slots, [])
            (List.mapi (fun index (guard, hole) -> (index, guard, hole)) (List.combine guards holes))))
  in
  let slots = guard_slots + List.length value_slots in
  let frame_type =
    lazy
    (Hoist.declare ~loc "frame" (fun frame_type ->
        type_declaration ~loc ~name:{ txt = frame_type; loc }
          ~params:(List.mapi (fun index _ -> (ptyp_var ~loc ("melhole_" ^ string_of_int index), (NoVariance, NoInjectivity))) creators)
          ~cstrs:[]
          ~kind:
            (Ptype_record
               (List.mapi
                  (fun index (named, _) ->
                    label_declaration ~loc
                      ~name:{ txt = frame_type ^ String.sub named 9 (String.length named - 9); loc }
                      ~mutable_:Immutable
                      ~type_:(ptyp_var ~loc ("melhole_" ^ string_of_int index)))
                  creators
               @ List.init slots (fun index ->
                     label_declaration ~loc
                       ~name:{ txt = frame_type ^ "_slot_" ^ string_of_int index; loc }
                       ~mutable_:Mutable ~type_:[%type: Melhole.View.input])))
          ~private_:Public ~manifest:None))
  in
  let field named = { txt = Lident (Lazy.force frame_type ^ String.sub named 9 (String.length named - 9)); loc } in
  let slot_field index = { txt = Lident (Lazy.force frame_type ^ "_slot_" ^ string_of_int index); loc } in
  let frame = [%expr __melhole_frame] in
  let read named = pexp_field ~loc frame (field named) in
  let remember index value = pexp_setfield ~loc frame (slot_field index) [%expr Melhole.View.Input [%e value]] in
  let remembered index = pexp_field ~loc frame (slot_field index) in
  let flag_name index = Printf.sprintf "__melhole_changed_%d" index in
  let flag index = evar ~loc (flag_name index) in
  let slot name =
    let rec find index names =
      match names with
      | [] -> constant
      | other :: rest -> if String.equal other name then index else find (index + 1) rest
    in
    find 0 variables
  in
  let sets =
    List.mapi
      (fun index (guard, (hole : expression Markup.hole)) ->
        let hole_value = evar ~loc (hole_name index) in
        let written put write value =
          Option.fold ~none:(write value)
            ~some:(fun at ->
              [%expr
                let __melhole_value = [%e value] in
                [%e put (remembered at) [%expr __melhole_value]];
                [%e remember at [%expr __melhole_value]]])
            (List.assoc_opt index value_slots)
        in
        let set =
          match hole with
          | On_element (_, Attr name, value) ->
              written
                (fun last value -> [%expr Melhole.View.put_attr [%e last] [%e hole_value] [%e estring ~loc name] [%e value]])
                (fun value -> [%expr Melhole.View.write_attr [%e hole_value] [%e estring ~loc name] [%e value]])
                value
          | At_position (_, _, Text_slot, value) ->
              written
                (fun last value -> [%expr Melhole.View.put_text [%e last] [%e hole_value] [%e value]])
                (fun value -> [%expr Melhole.Dom_api.set_data [%e hole_value] [%e value]])
                value
          | On_element (_, Sole_text, value) ->
              written
                (fun last value -> [%expr Melhole.View.put_sole_text [%e last] [%e hole_value] [%e value]])
                (fun value -> [%expr Melhole.View.write_sole_text [%e hole_value] [%e value]])
                value
          | On_element (_, Handler _, value) -> (
              let key = evar ~loc (key_name index) in
              match (frontend.handlers, guard) with
              | Linked dispatch, Free.Reads names ->
                  let data = List.filter (fun name -> not (String.equal name dispatch)) names in
                  let pattern, carried =
                    match data with
                    | [] -> ([%pat? ()], [%expr ()])
                    | [ single ] -> (pvar ~loc single, evar ~loc single)
                    | _ :: _ :: _ ->
                        (ppat_tuple ~loc (List.map (pvar ~loc) data), pexp_tuple ~loc (List.map (evar ~loc) data))
                  in
                  let body =
                    pexp_record ~loc
                      [
                        ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I3"); loc },
                          [%expr
                            fun __melhole_data [%p pvar ~loc dispatch] __melhole_event ->
                              (let [%p pattern] = __melhole_data in
                               [%e value] __melhole_event)
                              [@ocaml.warning "-26-27"]] );
                      ]
                      None
                  in
                  let site = Hoist.hoist ~loc "link" [%expr Melhole.View.link [%e body]] in
                  [%expr Melhole.View.put_linked [%e hole_value] [%e key] [%e site] [%e evar ~loc dispatch] [%e carried]]
              | Linked _, Free.Volatile | Closures, _ -> [%expr Melhole.View.put_handler [%e hole_value] [%e key] [%e value]])
          | On_element (_, (Prop _ | Control _), value) -> [%expr Melhole.View.set_prop [%e hole_value] [%e value]]
          | On_element (_, Ref, { pexp_desc = Pexp_tuple [ identity; callback ]; _ }) ->
              [%expr Melhole.View.set_reference [%e hole_value] [%e identity] [%e callback]]
          | On_element (_, Ref, value) ->
              [%expr
                let __melhole_reference = [%e value] in
                Melhole.View.set_reference [%e hole_value] __melhole_reference __melhole_reference]
          | On_element (_, Style, value) -> [%expr Melhole.View.set_style [%e hole_value] [%e value]]
          | At_position (_, _, Child_slot, value) -> [%expr Melhole.View.set_content [%e hole_value] [%e value]]
        in
        match guard with
        | Free.Volatile -> set
        | Free.Reads names ->
            let flags = match names with [] -> [ flag constant ] | _ :: _ -> List.map (fun name -> flag (slot name)) names in
            let unchanged =
              ppat_tuple ~loc
                (List.map (fun _ -> [%pat? Melhole.View.Unchanged]) flags)
            in
            pexp_match ~loc (pexp_tuple ~loc flags)
              [
                case ~lhs:unchanged ~guard:None ~rhs:[%expr ()];
                case ~lhs:[%pat? _] ~guard:None ~rhs:set;
              ])
      (List.combine guards holes)
  in
  let checks =
    List.mapi (fun index name -> (index, evar ~loc name)) variables
    @ Option.fold ~none:[] ~some:(fun index -> [ (index, [%expr ()]) ]) mounted
  in
  let body =
    List.fold_right
      (fun (index, value) body ->
        [%expr
          let [%p pvar ~loc (flag_name index)] = Melhole.View.check [%e remembered index] [%e value] in
          [%e remember index value];
          [%e body]])
      checks
      (List.fold_right (fun set rest -> [%expr [%e set]; [%e rest]]) sets [%expr ()])
  in
  let unary lambda = pexp_record ~loc [ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), "I1"); loc }, lambda) ] None in
  let binary lambda = pexp_record ~loc [ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), "I2"); loc }, lambda) ] None in
  let instance =
  match (values, role) with
  | [], `Child ->
      Hoist.hoist ~loc "element"
        (frontend.wrap loc (Hoist.hoist ~loc "static" [%expr Melhole.View.static [%e markup]]))
  | _, (`Row _ | `Child) ->
      let slot_names =
        List.concat
          (List.mapi
             (fun index (hole : expression Markup.hole) ->
               match hole with
               | At_position _ -> [ hole_name index ]
               | On_element _ -> [])
             holes)
      in
      let is_slot name = List.exists (String.equal name) slot_names in
      let from name = "__melhole_from" ^ name in
      let record fields =
        pexp_record ~loc
          (List.map (fun (named, value) -> (field named, value)) fields
          @ List.init slots (fun index -> (slot_field index, [%expr Melhole.View.unset])))
          None
      in
      let made = walked (effected (record creators)) in
      let duty statements =
        match statements with
        | [] -> [%expr None]
        | first :: rest ->
            [%expr
              Some
                [%e
                  unary
                    [%expr
                      fun __melhole_frame ->
                        [%e List.fold_left (fun done_ next -> [%expr [%e done_]; [%e next]]) first rest]]]]
      in
      let unlistened =
        List.concat
          (List.mapi
             (fun index (hole : expression Markup.hole) ->
               match hole with
               | On_element (_, Handler event, _) ->
                   [ [%expr Melhole.Events.unlisten [%e read (original (hole_name index))] [%e estring ~loc event]] ]
               | On_element (_, (Attr _ | Prop _ | Control _ | Ref | Style | Sole_text), _) | At_position _ -> [])
             holes)
      in
      let duties =
        [%expr
          {
            Melhole.View.placed =
              [%e
                duty
                  (List.filter_map
                     (fun (named, creator) ->
                       match creator with
                       | [%expr Melhole.View.reference_hole [%e? _]] -> Some [%expr Melhole.View.place_reference [%e read named]]
                       | _ -> None)
                     creators)];
            released =
              [%e
                duty
                  (unlistened
                  @ List.filter_map
                      (fun (named, creator) ->
                        match creator with
                        | [%expr Melhole.View.reference_hole [%e? _]] -> Some [%expr Melhole.View.release_reference [%e read named]]
                        | [%expr Melhole.View.content_hole [%e? _]] -> Some [%expr Melhole.View.release_content [%e read named]]
                        | _ -> None)
                      creators)];
            forgotten =
              [%e
                duty
                  (match frontend.throwing with
                  | Stops -> []
                  | Recovers -> List.init slots (fun index -> remember index [%expr Melhole.View.unset]))];
          }]
      in
      let cursor indices =
        "__melhole_cursor" ^ String.concat "" (List.map (fun index -> "_" ^ string_of_int index) indices)
      in
      let parent indices = List.filteri (fun at _ -> at < List.length indices - 1) indices in
      let hole_of value =
        let rec find index holes =
          match holes with
          | [] -> Location.raise_errorf ~loc "%s: a slot without a hole" frontend.name
          | (hole : expression Markup.hole) :: rest -> (
              match hole with
              | At_position (_, _, _, other) when other == value -> index
              | At_position _ | On_element _ -> find (index + 1) rest)
        in
        find 0 holes
      in
      let adopt =
        lazy
          (let finished =
             effected
               (record
                  (List.map
                     (fun (named, creator) -> match is_slot named with true -> (named, evar ~loc named) | false -> (named, creator))
                     creators))
           in
           let adopting =
             List.fold_right
               (fun (step : expression Markup.adoption) body ->
                 match step with
                 | Enter ([], _) -> [%expr let [%p pvar ~loc (cursor [])] = Melhole.View.inside __melhole_node in [%e body]]
                 | Enter (indices, tag) ->
                     [%expr
                       let [%p pvar ~loc (name indices)] =
                         Melhole.View.adopt_element [%e evar ~loc (cursor (parent indices))] [%e estring ~loc tag]
                       in
                       let [%p pvar ~loc (cursor indices)] = Melhole.View.inside [%e node indices] in
                       [%e body]]
                 | Leave indices -> [%expr Melhole.View.adopt_finish [%e evar ~loc (cursor indices)]; [%e body]]
                 | Static (indices, text) ->
                     [%expr Melhole.View.adopt_static [%e evar ~loc (cursor indices)] [%e estring ~loc text]; [%e body]]
                 | Anchor indices -> [%expr Melhole.View.adopt_anchor [%e evar ~loc (cursor indices)]; [%e body]]
                 | Slot_at (indices, Text_slot, value) ->
                     let named = hole_name (hole_of value) in
                     [%expr
                       let [%p pvar ~loc named] =
                         Melhole.View.adopt_text [%e evar ~loc (cursor indices)]
                           (Melhole.View.collected_text [%e evar ~loc (from named)])
                       in
                       [%e body]]
                 | Slot_at (indices, Child_slot, value) ->
                     let named = hole_name (hole_of value) in
                     [%expr
                       let [%p pvar ~loc named] =
                         [%e
                           Option.fold
                             ~none:
                               [%expr
                                 Melhole.View.adopt_content [%e evar ~loc (cursor indices)]
                                   (Melhole.View.collected [%e evar ~loc (from named)])
                                   Melhole.View.Before_next]
                             ~some:(fun path ->
                               [%expr
                                 Melhole.View.adopt_forked [%e evar ~loc (cursor indices)]
                                   (Melhole.View.collected [%e evar ~loc (from named)])
                                   Melhole.View.Before_next [%e path]])
                             (fork_path value)]
                       in
                       [%e body]]
                 | Child_before_anchor (indices, value) ->
                     let named = hole_name (hole_of value) in
                     [%expr
                       let [%p pvar ~loc named] =
                         [%e
                           Option.fold
                             ~none:
                               [%expr
                                 Melhole.View.adopt_content [%e evar ~loc (cursor indices)]
                                   (Melhole.View.collected [%e evar ~loc (from named)])
                                   Melhole.View.Before_anchor]
                             ~some:(fun path ->
                               [%expr
                                 Melhole.View.adopt_forked [%e evar ~loc (cursor indices)]
                                   (Melhole.View.collected [%e evar ~loc (from named)])
                                   Melhole.View.Before_anchor [%e path]])
                             (fork_path value)]
                       in
                       [%e body]])
               (Markup.adoption element) finished
           in
           let collected =
             List.fold_right
               (fun (named, _) body ->
                 match is_slot named with
                 | true ->
                     [%expr
                       let [%p pvar ~loc (from named)] = [%e pexp_field ~loc [%expr __melhole_collected] (field named)] in
                       [%e body]]
                 | false -> body)
               creators adopting
           in
           binary [%expr fun __melhole_node __melhole_collected -> ([%e collected] [@ocaml.warning "-26-27"])])
      in
      let template =
        Hoist.hoist ~loc "template"
          (match !output with
          | Hydratable ->
              [%expr
                Melhole.View.hydratable [%e markup]
                  [%e unary [%expr fun [%p pvar ~loc (name [])] -> [%e made]]]
                  [%e duties] [%e Lazy.force adopt]]
          | Client_only | Server ->
              [%expr Melhole.View.template [%e markup] [%e unary [%expr fun [%p pvar ~loc (name [])] -> [%e made]]] [%e duties]])
      in
      let refreshing =
        List.fold_right
          (fun (named, _) body -> [%expr let [%p pvar ~loc named] = [%e read named] in [%e body]])
          creators (bound body)
      in
      let refresh = unary [%expr fun __melhole_frame -> ([%e refreshing] [@ocaml.warning "-26-27"])] in
      match role with
      | `Row key ->
          let root = Hoist.hoist ~loc "root" (unary [%expr fun __melhole_frame -> [%e read root_name]]) in
          [%expr Melhole.View.row [%e template] [%e root] [%e refresh] ([%e key] : string)]
      | `Child -> frontend.wrap loc [%expr Melhole.View.compiled [%e template] [%e refresh]]
  in
  match role with
  | `Row _ -> instance
  | `Child ->
      Option.fold ~none:instance
        ~some:(fun key ->
          frontend.wrap loc
            [%expr Melhole.View.with_key ([%e key] : string) [%e frontend.unwrap loc instance]])
        key

type numbering = Items | Numbered

let loop frontend ~loc ~definitions ~view ~stable ~render ~numbering row items =
  match (!output, Free.closure ~definitions ~locals:frontend.locals row) with
  | Server, _ -> None
  | (Client_only | Hydratable), closure ->
  match closure with
  | Volatile -> None
  | Reads names ->
      let inputs values =
        match stable @ values with
        | [] -> [%expr Melhole.View.no_inputs]
        | given -> pexp_array ~loc (List.map (fun value -> [%expr Melhole.View.Input [%e value]]) given)
      in
      let uncurried lambda =
        pexp_record ~loc [ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), "I1"); loc }, lambda) ] None
      in
      let split =
        Option.bind
          (match row.pexp_desc with
          | Pexp_ident { txt = Lident name; _ } -> List.assoc_opt name definitions
          | _ -> Some row)
          (fun lambda ->
            let bound =
              match (numbering, lambda.pexp_desc) with
              | Items, Pexp_function ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], None, Pfunction_body body)
                ->
                  Some ((fun inner -> [%expr let [%p pattern] = __melhole_item in [%e inner]]), body, Free.bound pattern)
              | ( Numbered,
                  Pexp_function
                    ( [
                        { pparam_desc = Pparam_val (Nolabel, None, position); _ };
                        { pparam_desc = Pparam_val (Nolabel, None, pattern); _ };
                      ],
                      None,
                      Pfunction_body body ) ) ->
                  Some
                    ( (fun inner -> [%expr let [%p position] = __melhole_index in let [%p pattern] = __melhole_item in [%e inner]]),
                      body,
                      Free.bound position @ Free.bound pattern )
              | (Items | Numbered), _ -> None
            in
            Option.bind bound (fun (bind, body, row) ->
                match Free.of_expression lambda with
                | Volatile -> None
                | Reads read ->
                    let outer = List.filter (fun name -> List.mem name frontend.locals) read in
                    Option.map (fun leaves -> (bind, leaves, row, outer)) (Free.leaves ~outer ~view body)))
      in
      let written values =
        List.fold_right
          (fun (index, value) rest ->
            [%expr
              Stdlib.Array.unsafe_set __melhole_out (__melhole_at + [%e eint ~loc index]) (Melhole.View.Input [%e value]);
              [%e rest]])
          (List.mapi (fun index value -> (index, value)) (stable @ values))
          [%expr ()]
      in
      let pieces =
        eint ~loc
          (List.length stable
          + Option.fold ~none:(List.length names) ~some:(fun (_, leaves, _, _) -> List.length leaves) split)
      in
      let project =
        pexp_record ~loc
          [
            ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I4"); loc },
              Option.fold
                ~none:[%expr fun _ _ __melhole_out __melhole_at -> [%e written (List.map (fun name -> evar ~loc name) names)]]
                ~some:(fun (bind, leaves, _, _) ->
                  [%expr
                    fun __melhole_item __melhole_index __melhole_out __melhole_at ->
                      [%e bind (written leaves)]
                      [@ocaml.warning "-26-27"]])
                split );
          ]
          None
      in
      let outer = inputs (List.map (fun name -> evar ~loc name) names) in
      let selectors =
        let given = stable @ List.map (fun name -> evar ~loc name) names in
        let read leaf =
          match Free.closure ~definitions ~locals:frontend.locals leaf with
          | Volatile -> (1 lsl List.length given) - 1
          | Reads read ->
              List.fold_left ( lor ) 0
                (List.mapi
                   (fun at value ->
                     match value.pexp_desc with
                     | Pexp_ident { txt = Lident name; _ } when List.mem name read -> 1 lsl at
                     | _ -> 0)
                   given)
        in
        Option.fold ~none:[%expr Melhole.View.Unselecting]
          ~some:(fun (bind, leaves, row, outer) ->
            let pieces = List.map (fun leaf -> (leaf, Selector.of_leaf ~outer ~row leaf)) leaves in
            match (List.filter_map snd pieces, List.length given <= 30) with
            | [], _ | _, false -> [%expr Melhole.View.Unselecting]
            | chosen, true ->
                let others =
                  List.fold_left
                    (fun others (leaf, selector) ->
                      Option.fold ~none:(others lor read leaf) ~some:(fun _ -> others) selector)
                    ((1 lsl List.length stable) - 1)
                    pieces
                in
                let selection = Hoist.hoist ~loc "selection" [%expr Melhole.View.selection [%e eint ~loc others]] in
                [%expr
                  Melhole.View.Selecting
                    ( [%e selection],
                      [%e
                        pexp_array ~loc
                          (List.map
                             (fun { Selector.chosen; key; equality } ->
                               [%expr
                                 Melhole.View.selector
                                   [%e
                                     match equality with
                                     | Structural -> [%expr Melhole.View.Structural]
                                     | Physical -> [%expr Melhole.View.Physical]]
                                   [%e chosen]
                                   [%e
                                     pexp_record ~loc
                                       [
                                         ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I2"); loc },
                                           [%expr
                                             fun __melhole_item __melhole_index ->
                                               [%e bind key]
                                               [@ocaml.warning "-26-27"]] );
                                       ]
                                       None]])
                             chosen)] )])
          split
      in
      let rec marked body =
        match body.pexp_desc with
        | Pexp_let (flag, bindings, rest) ->
            Option.map (fun rest -> { body with pexp_desc = Pexp_let (flag, bindings, rest) }) (marked rest)
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident _; _ }; _ }, arguments)
          when Option.is_some (without_jsx body.pexp_attributes)
               && List.exists (fun (label, _) -> match label with Labelled "key" -> true | Nolabel | Labelled _ | Optional _ -> false) arguments ->
            Some
              {
                body with
                pexp_attributes =
                  attribute ~loc ~name:{ txt = row_marker; loc } ~payload:(PStr []) :: body.pexp_attributes;
              }
        | _ -> None
      in
      let compiled_rows =
        Option.bind
          (match (numbering, row.pexp_desc) with
          | Numbered, _ -> None
          | Items, Pexp_ident { txt = Lident name; _ } -> List.assoc_opt name definitions
          | Items, _ -> Some row)
          (fun lambda ->
            match lambda.pexp_desc with
            | Pexp_function (([ { pparam_desc = Pparam_val (Nolabel, None, _); _ } ] as parameters), None, Pfunction_body body)
              ->
                Option.bind (marked body) (fun body ->
                    let lambda = { lambda with pexp_desc = Pexp_function (parameters, None, Pfunction_body body) } in
                    let mapped = frontend.map lambda in
                    let split =
                      match mapped.pexp_desc with
                      | Pexp_function
                          ([ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ], None, Pfunction_body body) ->
                          let rec tail body =
                            match body.pexp_desc with
                            | Pexp_let (flag, bindings, rest) ->
                                Option.map
                                  (fun (rebuild, found) ->
                                    ((fun inner -> { body with pexp_desc = Pexp_let (flag, bindings, rebuild inner) }), found))
                                  (tail rest)
                            | Pexp_apply
                                ( { pexp_desc = Pexp_ident { txt = Ldot (Ldot (Lident "Melhole", "View"), "row"); _ }; _ },
                                  [
                                    (Nolabel, template);
                                    (Nolabel, root);
                                    (Nolabel, { pexp_desc = Pexp_record ([ (_, refresh) ], None); _ });
                                    (Nolabel, key);
                                  ] ) -> (
                                match refresh.pexp_desc with
                                | Pexp_function (parameters, None, Pfunction_body refreshing) ->
                                    Some ((fun inner -> inner), (template, root, parameters, refreshing, key))
                                | _ -> None)
                            | _ -> None
                          in
                          Option.map (fun (rebuild, parts) -> (pattern, rebuild, parts)) (tail body)
                      | _ -> None
                    in
                    Option.map
                      (fun (pattern, rebuild, (template, root, parameters, refreshing, key)) ->
                        let given body = [%expr (let [%p pattern] = __melhole_item in [%e rebuild body]) [@ocaml.warning "-26-27"]] in
                        let refresh =
                          pexp_record ~loc
                            [
                              ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I2"); loc },
                                pexp_function ~loc
                                  (parameters @ [ { pparam_loc = loc; pparam_desc = Pparam_val (Nolabel, None, [%pat? __melhole_item]) } ])
                                  None (Pfunction_body (given refreshing)) );
                            ]
                            None
                        in
                        let loop =
                          [%expr
                            Melhole.View.rows [%e items] [%e template] [%e root] [%e refresh]
                              [%e uncurried [%expr fun __melhole_item -> [%e given key]]]
                              [%e project] [%e pieces] [%e outer] [%e selectors]]
                        in
                        match row.pexp_desc with
                        | Pexp_ident _ -> [%expr let _ = [%e row] in [%e loop]]
                        | _ -> loop)
                      split)
            | _ -> None)
      in
      match compiled_rows with
      | Some rows -> Some rows
      | None ->
          Some
            [%expr
              Melhole.View.each
                [%e Hoist.hoist ~loc "site" [%expr Melhole.View.loop_site ()]]
                [%e match numbering with Items -> [%expr Melhole.View.Items] | Numbered -> [%expr Melhole.View.Numbered]]
                [%e items]
                [%e
                  pexp_record ~loc
                    [
                      ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I2"); loc },
                        match numbering with
                        | Items ->
                            [%expr fun __melhole_item _ -> [%e render [%expr [%e frontend.map row] __melhole_item]]]
                        | Numbered ->
                            [%expr
                              fun __melhole_item __melhole_index ->
                                [%e render [%expr [%e frontend.map row] __melhole_index __melhole_item]]] );
                    ]
                    None]
                [%e project] [%e pieces] [%e outer] [%e selectors]]

let fragment frontend ~loc items =
  let children = List.map (fun item -> frontend.unwrap loc (frontend.map item)) items in
  match (!output, frontend.ids, children) with
  | Server, Tree_ids, [ only ] -> frontend.wrap loc only
  | Server, Tree_ids, _ -> frontend.wrap loc [%expr Melhole_server.array [%e pexp_array ~loc children]]
  | Server, No_ids, _ -> frontend.wrap loc [%expr Melhole_server.list [%e elist ~loc children]]
  | (Client_only | Hydratable), _, _ -> frontend.wrap loc [%expr Melhole.View.list [%e elist ~loc children]]
