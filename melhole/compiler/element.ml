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
  style : location -> [ `Required | `Optional ] -> expression -> expression;
  reference : location -> [ `Required | `Optional ] -> expression -> expression;
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
  match (kind, requirement, literal) with
  | Dom_props.Ignored, _, _ -> Absent
  | Attribute (name, Text), `Required, `String text -> Static (name, Valued text)
  | Attribute (name, Text), `Required, _ ->
      hole (Attr name) [%expr (Some ([%e value] : string))]
  | Attribute (name, Flag), `Required, `True -> Static (name, Bare)
  | Attribute (_, Flag), `Required, `False -> Absent
  | Attribute (name, Flag), `Required, _ ->
      hole (Attr name)
        [%expr (if ([%e value] : bool) then Some "" else None)]
  | Attribute (name, Booleanish), `Required, `True -> Static (name, Valued "true")
  | Attribute (name, Booleanish), `Required, `False -> Static (name, Valued "false")
  | Attribute (name, Booleanish), `Required, _ ->
      hole (Attr name)
        [%expr
          (Some (if ([%e value] : bool) then "true" else "false"))]
  | Attribute (name, (Integer | Decimal)), `Required, `Number digits ->
      Static (name, Valued digits)
  | Attribute (name, Integer), `Required, _ ->
      hole (Attr name)
        [%expr (Some (string_of_int ([%e value] : int)))]
  | Attribute (name, Decimal), `Required, _ ->
      hole (Attr name)
        [%expr (Some ([%e float_text ~loc] ([%e value] : float)))]
  | Attribute (name, Text), `Optional, _ ->
      hole (Attr name) [%expr ([%e value] : string option)]
  | Attribute (name, Flag), `Optional, _ ->
      hole (Attr name)
        [%expr
          
            (match ([%e value] : bool option) with
            | Some true -> Some ""
            | Some false | None -> None)]
  | Attribute (name, Booleanish), `Optional, _ ->
      hole (Attr name)
        [%expr
          
            (Stdlib.Option.map
               (fun flag -> if flag then "true" else "false")
               ([%e value] : bool option))]
  | Attribute (name, Integer), `Optional, _ ->
      hole (Attr name)
        [%expr (Stdlib.Option.map string_of_int ([%e value] : int option))]
  | Attribute (name, Decimal), `Optional, _ ->
      hole (Attr name)
        [%expr
          (Stdlib.Option.map [%e float_text ~loc] ([%e value] : float option))]
  | Property (name, kind), requirement, _ -> property (Prop name) kind requirement
  | Controlled (name, kind), requirement, _ -> property (Control name) kind requirement
  | Handler event, requirement, _ ->
      hole (Handler event) (frontend.handler loc { label; event; requirement; value })
  | Style, requirement, _ -> hole Style (frontend.style loc requirement value)
  | Ref, requirement, _ -> hole Ref (frontend.reference loc requirement value)
  | Inner_html, `Required, _ -> hole (Prop "innerHTML") [%expr Melhole.View.html_of [%e value]]
  | Inner_html, `Optional, _ ->
      hole (Prop "innerHTML") [%expr Stdlib.Option.fold ~none:"" ~some:Melhole.View.html_of [%e value]]

let rec analyse frontend ~loc tag arguments =
  let statics = ref [] and dynamics = ref [] and children = ref [] and key = ref None in
  List.iter
    (fun (label, value) ->
      match label with
      | Nolabel -> ()
      | Labelled "key" -> key := Some (frontend.map value)
      | Labelled "children" ->
          children :=
            (match list_items value with
            | Some items -> List.filter_map (child frontend) items
            | None ->
                [
                  Markup.Slot
                    ( Child_slot,
                      [%expr [%e frontend.unwrap loc (frontend.map value)]] );
                ])
      | Labelled name | Optional name -> (
          let requirement =
            match label with Optional _ -> `Optional | Labelled _ | Nolabel -> `Required
          in
          match
            match Dom_props.Prop.of_string name with
            | Some parsed -> Some (Dom_props.kind parsed)
            | None -> frontend.extra name
          with
          | None ->
              Location.raise_errorf ~loc:value.pexp_loc "%s: <%s> has no prop %s" frontend.name tag
                name
          | Some kind -> (
              match prop frontend ~loc ~label:name kind requirement (frontend.map value) with
              | Static (attribute, static) -> statics := (attribute, static) :: !statics
              | Dynamic (site, hole) -> dynamics := (site, hole) :: !dynamics
              | Absent -> ())))
    arguments;
  (match (Markup.void tag, !children) with
  | `Void, _ :: _ -> Location.raise_errorf ~loc "%s: <%s> cannot have children" frontend.name tag
  | `Void, [] | `Container, _ -> ());
  ( { Markup.tag; statics = List.rev !statics; dynamics = List.rev !dynamics; children = !children },
    !key )

and child frontend item =
  let loc = item.pexp_loc in
  match lowercase_call item with
  | Some (tag, arguments) -> Some (Markup.Element (fst (analyse frontend ~loc tag arguments)))
  | None -> (
      match frontend.classify item with
      | Some Nothing -> None
      | Some (Text (String_text, { pexp_desc = Pexp_constant (Pconst_string (text, _, _)); _ })) ->
          Some (Markup.Text text)
      | Some
          (Text
             ( (Int_text | Float_text),
               {
                 pexp_desc =
                   Pexp_constant (Pconst_integer (digits, None) | Pconst_float (digits, None));
                 _;
               } )) ->
          Some (Markup.Text digits)
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
                 match frontend.each loc item with
                 | Some loop -> loop
                 | None -> frontend.unwrap loc (frontend.map item) )))

let row_marker = "melhole.row"

let served frontend ~loc (element : expression Markup.element) key =
  let writer = [%expr __melhole_writer] in
  let checks = ref [] in
  let rec ops (tree : expression Markup.element) =
    let opening =
      "<" ^ tree.tag
      ^ String.concat ""
          (List.map
             (fun (name, static) ->
               match static with
               | Markup.Valued value -> " " ^ name ^ "=\"" ^ Melhole_server.escape value ^ "\""
               | Bare -> " " ^ name ^ "=\"\"")
             tree.statics)
    in
    let content = ref None in
    let attributes =
      List.filter_map
        (fun ((site : Markup.site), value) ->
          match (site, tree.tag) with
          | Attr name, _ -> Some (`Run [%expr Melhole_server.attribute [%e writer] [%e estring ~loc name] [%e value]])
          | Prop "innerHTML", _ ->
              content := Some [%expr Melhole_server.raw [%e writer] [%e value]];
              None
          | (Prop "value" | Control "value"), "textarea" ->
              content := Some [%expr Melhole_server.dynamic_text [%e writer] [%e value]];
              None
          | (Prop "value" | Control "value"), _ ->
              Some (`Run [%expr Melhole_server.attribute [%e writer] "value" (Some [%e value])])
          | (Prop (("checked" | "selected") as name) | Control (("checked" | "selected") as name)), _ ->
              Some (`Run [%expr Melhole_server.flag [%e writer] [%e estring ~loc name] [%e value]])
          | Style, _ -> Some (`Run [%expr Melhole_server.style [%e writer] [%e value]])
          | (Prop _ | Control _ | Handler _ | Ref | Sole_text), _ -> Some (`Typed [%expr ignore (fun () -> [%e value])]))
        tree.dynamics
    in
    List.iter (fun step -> match step with `Typed run -> checks := run :: !checks | `Run _ -> ()) attributes;
    let attributes = List.filter_map (fun step -> match step with `Run run -> Some (`Run run) | `Typed _ -> None) attributes in
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
            | Slot (Child_slot, value), _ -> ([ `Run [%expr Melhole_server.write [%e writer] [%e value]] ], `Unknown)
          in
          let later, last = children rest after in
          (now @ later, last)
    in
    let inner = Option.fold ~none:(fst (children tree.children `Other)) ~some:(fun run -> [ `Run run ]) !content in
    let tail = match Markup.void tree.tag with `Void -> [] | `Container -> [ `Tag ("</" ^ tree.tag ^ ">") ] in
    (`Tag opening :: attributes) @ (`Tag closing :: inner) @ tail
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
        in
        [%expr [%e run]; [%e rest]])
      (List.map (fun run -> `Run run) (List.rev !checks) @ merged steps)
      [%expr ()]
  in
  let instance = frontend.wrap loc [%expr Melhole_server.element (fun __melhole_writer -> [%e body])] in
  Option.fold ~none:instance
    ~some:(fun key -> [%expr ignore ([%e key] : string); [%e instance]])
    key

let lowercase frontend ~loc ~attributes tag arguments =
  let element, key = analyse frontend ~loc tag arguments in
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
  let scope = [%expr __melhole_scope] in
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
                (key_name index, [%expr Melhole.View.listen [%e scope] [%e at indices] [%e estring ~loc event]]);
              ],
              value )
        | At_position (indices, index_in, Text_slot, value) ->
            one [%expr Melhole.View.text_at [%e position indices index_in]] value
        | On_element (indices, Prop name, value) ->
            one [%expr Melhole.View.prop_hole [%e at indices] [%e estring ~loc name]] value
        | On_element (indices, Control name, value) ->
            one [%expr Melhole.View.controlled_hole [%e at indices] [%e estring ~loc name]] value
        | On_element (indices, Ref, value) -> one [%expr Melhole.View.reference_hole [%e scope] [%e at indices]] value
        | On_element (indices, Style, value) -> one [%expr Melhole.View.style_hole [%e at indices]] value
        | At_position (indices, index_in, Child_slot, value) ->
            one [%expr Melhole.View.content_hole [%e scope] [%e position indices index_in]] value)
      holes
  in
  let creators, same, effects, keys =
    let kept, same, effects, keys =
      List.fold_left
        (fun (kept, same, effects, keys) (named, creator) ->
          match creator with
          | [%expr Melhole.View.listen [%e? scope] [%e? element] [%e? event]] ->
              ( kept,
                same,
                [%expr Melhole.View.listen [%e scope] [%e element] [%e event]] :: effects,
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
        (List.concat_map fst made_holes)
    in
    (List.rev kept, List.rev same, List.rev effects, List.rev keys)
  in
  let effected body = List.fold_right (fun registered body -> [%expr [%e registered]; [%e body]]) effects body in
  let bound body =
    List.fold_right
      (fun (named, value) body -> [%expr let [%p pvar ~loc named] = [%e value] in [%e body]])
      (List.map (fun (named, original) -> (named, evar ~loc original)) same @ keys)
      body
  in
  let values = List.map snd made_holes in
  let tuple items = match items with [ single ] -> single | _ -> pexp_tuple ~loc items in
  let made =
    List.fold_right
      (fun (indices, (step : Markup.step)) body ->
        let reach =
          match step with
          | First_of parent -> [%expr Melhole.Template.first [%e node parent]]
          | Next_of previous -> [%expr Melhole.Template.next [%e node previous]]
        in
        [%expr let [%p pvar ~loc (name indices)] = [%e reach] in [%e body]])
      walk (effected (tuple (List.map snd creators)))
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
    List.rev
      (snd
         (List.fold_left
            (fun (next, found) (index, (hole : expression Markup.hole)) ->
              match hole with
              | On_element (_, (Attr _ | Sole_text), _) | At_position (_, _, Text_slot, _) -> (next + 1, (index, next) :: found)
              | On_element (_, (Prop _ | Control _ | Handler _ | Ref | Style), _) | At_position (_, _, Child_slot, _) ->
                  (next, found))
            (guard_slots, [])
            (List.mapi (fun index hole -> (index, hole)) holes)))
  in
  let slots = guard_slots + List.length value_slots in
  let value_slot index = eint ~loc (List.assoc index value_slots) in
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
        let set =
          match hole with
          | On_element (_, Attr name, value) ->
              [%expr
                Melhole.View.put_attr __melhole_scope [%e value_slot index] [%e hole_value] [%e estring ~loc name]
                  [%e value]]
          | At_position (_, _, Text_slot, value) ->
              [%expr Melhole.View.put_text __melhole_scope [%e value_slot index] [%e hole_value] [%e value]]
          | On_element (_, Sole_text, value) ->
              [%expr Melhole.View.put_sole_text __melhole_scope [%e value_slot index] [%e hole_value] [%e value]]
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
          let [%p pvar ~loc (flag_name index)] =
            Melhole.View.check [%e scope] [%e eint ~loc index] [%e value]
          in
          [%e body]])
      checks
      (List.fold_right (fun set rest -> [%expr [%e set]; [%e rest]]) sets [%expr ()])
  in
  let uncurried lambda =
    pexp_record ~loc [ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), "I2"); loc }, lambda) ] None
  in
  let instance =
    match (values, role) with
    | [], `Row key ->
        let template =
          Hoist.hoist ~loc "template"
            [%expr Melhole.View.template [%e markup] [%e uncurried [%expr fun _ _ -> ()]]]
        in
        [%expr Melhole.View.row [%e template] 0 [%e uncurried [%expr fun _ () -> ()]] ([%e key] : string)]
    | [], `Child ->
        Hoist.hoist ~loc "element"
          (frontend.wrap loc (Hoist.hoist ~loc "static" [%expr Melhole.View.static [%e markup]]))
    | _ :: _, _ ->
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
        let finished =
          effected
            (tuple (List.map (fun (named, creator) -> match is_slot named with true -> evar ~loc named | false -> creator) creators))
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
                      Melhole.View.adopt_content __melhole_scope [%e evar ~loc (cursor indices)]
                        (Melhole.View.collected [%e evar ~loc (from named)])
                        Melhole.View.Before_next
                    in
                    [%e body]]
              | Child_before_anchor (indices, value) ->
                  let named = hole_name (hole_of value) in
                  [%expr
                    let [%p pvar ~loc named] =
                      Melhole.View.adopt_content __melhole_scope [%e evar ~loc (cursor indices)]
                        (Melhole.View.collected [%e evar ~loc (from named)])
                        Melhole.View.Before_anchor
                    in
                    [%e body]])
            (Markup.adoption element) finished
        in
        let collected_pattern =
          match
            List.map (fun (named, _) -> match is_slot named with true -> pvar ~loc (from named) | false -> ppat_any ~loc) creators
          with
          | [ single ] -> single
          | patterns -> ppat_tuple ~loc patterns
        in
        let adopt =
          lazy
          (pexp_record ~loc
            [
              ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I3"); loc },
                [%expr
                  fun __melhole_scope __melhole_node __melhole_collected ->
                    (let [%p collected_pattern] = __melhole_collected in
                     [%e adopting])
                    [@ocaml.warning "-26-27"]] );
            ]
            None)
        in
        let template =
          Hoist.hoist ~loc "template"
            (match !output with
            | Hydratable ->
                [%expr
                  Melhole.View.hydratable [%e markup]
                    [%e uncurried [%expr fun __melhole_scope [%p pvar ~loc (name [])] -> [%e made]]]
                    [%e Lazy.force adopt]]
            | Client_only | Server ->
                [%expr
                  Melhole.View.template [%e markup]
                    [%e uncurried [%expr fun __melhole_scope [%p pvar ~loc (name [])] -> [%e made]]]])
        in
        let holes =
          match List.map (fun (name, _) -> pvar ~loc name) creators with
          | [ single ] -> single
          | patterns -> ppat_tuple ~loc patterns
        in
        let refresh = uncurried [%expr fun __melhole_scope [%p holes] -> [%e bound body]] in
        match role with
        | `Row key -> [%expr Melhole.View.row [%e template] [%e eint ~loc slots] [%e refresh] ([%e key] : string)]
        | `Child -> frontend.wrap loc [%expr Melhole.View.compiled [%e template] [%e eint ~loc slots] [%e refresh]]
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
        pexp_array ~loc
          (List.map (fun value -> [%expr Melhole.View.Input [%e value]]) (stable @ values))
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
                  Some ((fun inner -> [%expr let [%p pattern] = __melhole_item in [%e inner]]), body)
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
                      body )
              | (Items | Numbered), _ -> None
            in
            Option.bind bound (fun (bind, body) ->
                match Free.of_expression lambda with
                | Volatile -> None
                | Reads read ->
                    Option.map
                      (fun leaves -> (bind, leaves))
                      (Free.leaves
                         ~outer:(List.filter (fun name -> List.mem name frontend.locals) read)
                         ~view body)))
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
          + Option.fold ~none:(List.length names) ~some:(fun (_, leaves) -> List.length leaves) split)
      in
      let project =
        pexp_record ~loc
          [
            ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I4"); loc },
              Option.fold
                ~none:[%expr fun _ _ __melhole_out __melhole_at -> [%e written (List.map (fun name -> evar ~loc name) names)]]
                ~some:(fun (bind, leaves) ->
                  [%expr
                    fun __melhole_item __melhole_index __melhole_out __melhole_at ->
                      [%e bind (written leaves)]
                      [@ocaml.warning "-26-27"]])
                split );
          ]
          None
      in
      let outer = inputs (List.map (fun name -> evar ~loc name) names) in
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
                                    (Nolabel, slots);
                                    (Nolabel, { pexp_desc = Pexp_record ([ (_, refresh) ], None); _ });
                                    (Nolabel, key);
                                  ] ) -> (
                                match refresh.pexp_desc with
                                | Pexp_function (parameters, None, Pfunction_body refreshing) ->
                                    Some ((fun inner -> inner), (template, slots, parameters, refreshing, key))
                                | _ -> None)
                            | _ -> None
                          in
                          Option.map (fun (rebuild, parts) -> (pattern, rebuild, parts)) (tail body)
                      | _ -> None
                    in
                    Option.map
                      (fun (pattern, rebuild, (template, slots, parameters, refreshing, key)) ->
                        let given body = [%expr (let [%p pattern] = __melhole_item in [%e rebuild body]) [@ocaml.warning "-26-27"]] in
                        let refresh =
                          pexp_record ~loc
                            [
                              ( { txt = Ldot (Ldot (Lident "Js", "Fn"), "I3"); loc },
                                pexp_function ~loc
                                  (parameters @ [ { pparam_loc = loc; pparam_desc = Pparam_val (Nolabel, None, [%pat? __melhole_item]) } ])
                                  None (Pfunction_body (given refreshing)) );
                            ]
                            None
                        in
                        let loop =
                          [%expr
                            Melhole.View.rows [%e items] [%e template] [%e slots] [%e refresh]
                              [%e uncurried [%expr fun __melhole_item -> [%e given key]]]
                              [%e project] [%e pieces] [%e outer]]
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
                [%e project] [%e pieces] [%e outer]]

let fragment frontend ~loc items =
  frontend.wrap loc
    [%expr
      [%e match !output with Server -> [%expr Melhole_server.list] | Client_only | Hydratable -> [%expr Melhole.View.list]]
        [%e elist ~loc (List.map (fun item -> frontend.unwrap loc (frontend.map item)) items)]]
