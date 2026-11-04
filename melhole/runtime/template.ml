type ('element, 'position) hole =
  | Attr of 'element * string
  | Prop of 'element * string
  | Text of 'position
  | Child of 'position
  | Handler of 'element * string
  | Ref of 'element
  | Style of 'element
  | Class_list of 'element

type position = { parent : Dom.node; before : Dom.node Js.null }

(** The markup until the first clone parses it, then the parsed root. *)
type source = Markup of { markup : string; root : Dom.node -> Dom.node } | Parsed of Dom.element

type t = {
  mutable source : source;
  paths : (int list, int list * int) hole array;
}

external first : Dom.node -> Dom.node = "firstChild" [@@mel.get]
external next : Dom.node -> Dom.node = "nextSibling" [@@mel.get]

external first_or_end : Dom.node -> Dom.node Js.null = "firstChild" [@@mel.get]
external next_or_end : Dom.node -> Dom.node Js.null = "nextSibling" [@@mel.get]

external element : Dom.node -> Dom.element = "%identity"

let html markup = { source = Markup { markup; root = first }; paths = [||] }

let svg markup =
  { source = Markup { markup = "<svg>" ^ markup ^ "</svg>"; root = (fun content -> first (first content)) }; paths = [||] }

let make ~html ~holes = { source = Markup { markup = html; root = first }; paths = holes }

let clone t =
  match t.source with
  | Parsed root -> Dom_api.clone_node root
  | Markup { markup; root } ->
      let template = Dom_api.create_element Dom_api.document "template" in
      Dom_api.set_inner_html template markup;
      let parsed = root (Dom_api.to_node (Dom_api.template_content template)) in
      let rec unmark node =
        Option.iter unmark (Dom_api.first_child node);
        let following = Dom_api.next_sibling node in
        (match Dom_api.node_type node with
        | 8 ->
            Option.iter
              (fun parent ->
                Dom_api.replace_child parent (Dom_api.create_text_node Dom_api.document "") node)
              (Dom_api.parent_node node)
        | _ -> ());
        Option.iter unmark following
      in
      Option.iter unmark (Dom_api.first_child parsed);
      t.source <- Parsed (element parsed);
      Dom_api.clone_node (element parsed)

let locate t root =
  let fail () = Js.Exn.raiseError "melhole: a template path does not resolve" in
  let rec slot candidate index =
    match (candidate, index) with
    | _, 0 -> candidate
    | Some node, _ -> slot (Dom_api.next_sibling node) (index - 1)
    | None, _ -> fail ()
  in
  let rec descend node path =
    match path with
    | [] -> node
    | index :: rest -> (
        match slot (Dom_api.first_child node) index with
        | Some child -> descend child rest
        | None -> fail ())
  in
  let at path = element (descend root path) in
  let position (path, index) =
    let parent = descend root path in
    { parent; before = Js.Null.fromOption (slot (Dom_api.first_child parent) index) }
  in
  Array.map
    (fun hole ->
      match hole with
      | Attr (path, name) -> Attr (at path, name)
      | Prop (path, name) -> Prop (at path, name)
      | Text spot -> Text (position spot)
      | Child spot -> Child (position spot)
      | Handler (path, name) -> Handler (at path, name)
      | Ref path -> Ref (at path)
      | Style path -> Style (at path)
      | Class_list path -> Class_list (at path))
    t.paths
