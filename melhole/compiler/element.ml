open Ppxlib
open Ast_builder.Default

type text = String_text | Int_text | Float_text
type special = Text of text * expression | Nothing

type handler = {
  label : string;
  event : string;
  family : string;
  requirement : [ `Required | `Optional ];
  value : expression;
}

type frontend = {
  name : string;
  locals : string list;
  map : expression -> expression;
  wrap : location -> expression -> expression;
  unwrap : location -> expression -> expression;
  handler : location -> handler -> expression;
  style : location -> [ `Required | `Optional ] -> expression -> expression;
  reference : location -> [ `Required | `Optional ] -> expression -> expression;
  classify : expression -> special option;
  extra : string -> Dom_props.kind option;
}

(** What one DOM prop becomes. *)
type prop =
  | Static of string * Markup.static  (** Written into the template. *)
  | Dynamic of Markup.site * expression  (** A hole and its value. *)
  | Absent

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
          [%expr (Option.value ~default:[%e default] ([%e value] : [%t type_of kind] option))]
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
        [%expr (Some (Js.Float.toString ([%e value] : float)))]
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
          
            (Option.map
               (fun flag -> if flag then "true" else "false")
               ([%e value] : bool option))]
  | Attribute (name, Integer), `Optional, _ ->
      hole (Attr name)
        [%expr (Option.map string_of_int ([%e value] : int option))]
  | Attribute (name, Decimal), `Optional, _ ->
      hole (Attr name)
        [%expr
          (Option.map Js.Float.toString ([%e value] : float option))]
  | Property (name, kind), requirement, _ -> property (Prop name) kind requirement
  | Controlled (name, kind), requirement, _ -> property (Control name) kind requirement
  | Handler (event, family), requirement, _ ->
      hole (Handler event) (frontend.handler loc { label; event; family; requirement; value })
  | Style, requirement, _ -> hole Style (frontend.style loc requirement value)
  | Ref, requirement, _ -> hole Ref (frontend.reference loc requirement value)
  | Inner_html, `Required, _ ->
      hole (Prop "innerHTML") [%expr [%e value]##__html]
  | Inner_html, `Optional, _ ->
      hole (Prop "innerHTML")
        [%expr
          
            (Option.fold ~none:"" ~some:(fun html -> html##__html) [%e value])]

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
                   (Js.Float.toString ([%e frontend.map value] : float))] ))
      | Some (Text (String_text, value)) ->
          Some
            (Markup.Slot
               (Text_slot, [%expr ([%e frontend.map value] : string)]))
      | None ->
          Some
            (Markup.Slot
               ( Child_slot,
                 [%expr [%e frontend.unwrap loc (frontend.map item)]] )))

let lowercase frontend ~loc tag arguments =
  let element, key = analyse frontend ~loc tag arguments in
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
  let creators, values =
    List.split
      (List.map
         (fun (hole : expression Markup.hole) ->
           match hole with
           | On_element (indices, Attr name, value) ->
               ([%expr Melhole.View.attr_hole [%e at indices] [%e estring ~loc name]], value)
           | On_element (indices, Prop name, value) ->
               ([%expr Melhole.View.prop_hole [%e at indices] [%e estring ~loc name]], value)
           | On_element (indices, Control name, value) ->
               ([%expr Melhole.View.controlled_hole [%e at indices] [%e estring ~loc name]], value)
           | On_element (indices, Handler event, value) ->
               ( [%expr Melhole.View.handler_hole [%e scope] [%e at indices] [%e estring ~loc event]],
                 value )
           | On_element (indices, Ref, value) ->
               ([%expr Melhole.View.reference_hole [%e scope] [%e at indices]], value)
           | On_element (indices, Style, value) -> ([%expr Melhole.View.style_hole [%e at indices]], value)
           | At_position (indices, index, Text_slot, value) ->
               ([%expr Melhole.View.text_hole [%e position indices index]], value)
           | At_position (indices, index, Child_slot, value) ->
               ([%expr Melhole.View.content_hole [%e scope] [%e position indices index]], value))
         holes)
  in
  let hole_name index = "__melhole_hole_" ^ string_of_int index in
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
      walk (tuple creators)
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
        | On_element (_, (Handler _ | Ref), _) -> Free.Volatile
        | On_element (_, (Attr _ | Prop _ | Control _ | Style), value) | At_position (_, _, _, value) ->
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
  let slots = constant + Option.fold ~none:0 ~some:(fun _ -> 1) mounted in
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
        let writer, value =
          match hole with
          | On_element (_, Attr _, value) -> ("set_attr", value)
          | On_element (_, (Prop _ | Control _), value) -> ("set_prop", value)
          | On_element (_, Handler _, value) -> ("set_handler", value)
          | On_element (_, Ref, value) -> ("set_reference", value)
          | On_element (_, Style, value) -> ("set_style", value)
          | At_position (_, _, Text_slot, value) -> ("set_text", value)
          | At_position (_, _, Child_slot, value) -> ("set_content", value)
        in
        let set =
          [%expr
            [%e evar ~loc ("Melhole.View." ^ writer)] [%e evar ~loc (hole_name index)] [%e value]]
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
  let instance =
    match values with
    | [] -> Hoist.hoist ~loc "element" (frontend.wrap loc [%expr Melhole.View.static [%e markup]])
    | _ :: _ ->
        let template =
          Hoist.hoist ~loc "template"
            [%expr
              Melhole.View.template [%e markup] (fun __melhole_scope [%p pvar ~loc (name [])] ->
                  [%e made])]
        in
        let holes =
          match List.mapi (fun index _ -> pvar ~loc (hole_name index)) values with
          | [ single ] -> single
          | patterns -> ppat_tuple ~loc patterns
        in
        frontend.wrap loc
          [%expr
            Melhole.View.compiled [%e template] [%e eint ~loc slots] (fun __melhole_scope [%p holes] ->
                [%e body])]
  in
  Option.fold ~none:instance
    ~some:(fun key ->
      frontend.wrap loc
        [%expr Melhole.View.with_key ([%e key] : string) [%e frontend.unwrap loc instance]])
    key

let fragment frontend ~loc items =
  frontend.wrap loc
    [%expr
      Melhole.View.list
        [%e elist ~loc (List.map (fun item -> frontend.unwrap loc (frontend.map item)) items)]]
