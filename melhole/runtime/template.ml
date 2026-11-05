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

external first_child : Dom.node -> Dom.node = "firstChild" [@@mel.get]

let first_or_end node = Dom_api.first_child_or_end node
let next_or_end node = Dom_api.following_sibling node
let first node = Js.Null.getUnsafe (first_or_end node)
let next node = Js.Null.getUnsafe (next_or_end node)

external element : Dom.node -> Dom.element = "%identity"

let html markup = { source = Markup { markup; root = first_child }; paths = [||] }

let svg markup =
  { source = Markup { markup = "<svg>" ^ markup ^ "</svg>"; root = (fun content -> first_child (first_child content)) }; paths = [||] }

let make ~html ~holes = { source = Markup { markup = html; root = first_child }; paths = holes }

let parsed t =
  match t.source with
  | Parsed root -> root
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
      element parsed

let clone t = Dom_api.clone_node (parsed t)
let paths t = t.paths

let tag t = Dom_api.node_name (Dom_api.to_node (parsed t))

let locate t root =
  let fail () = Js.Exn.raiseError "melhole: a template path does not resolve" in
  let rec slot candidate index =
    match (Dom_api.same candidate Js.null, index) with
    | _, 0 -> candidate
    | false, _ -> slot (next_or_end (Js.Null.getUnsafe candidate)) (index - 1)
    | true, _ -> fail ()
  in
  let rec descend node path =
    match path with
    | [] -> node
    | index :: rest -> (
        let child = slot (first_or_end node) index in
        match Dom_api.same child Js.null with
        | false -> descend (Js.Null.getUnsafe child) rest
        | true -> fail ())
  in
  let at path = element (descend root path) in
  let position (path, index) =
    let parent = descend root path in
    { parent; before = slot (first_or_end parent) index }
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
