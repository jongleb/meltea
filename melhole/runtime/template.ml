type ('element, 'position) hole =
  | Attr of 'element * string
  | Prop of 'element * string
  | Text of 'position
  | Child of 'position
  | Handler of 'element * string
  | Ref of 'element
  | Style of 'element
  | Class_list of 'element

type position = { parent : Dom.node; before : Dom.node option }

type t = {
  html : string;
  holes : (int list, int list * int) hole array;
  root : Dom.element Lazy.t;
}

let element_of node =
  match Dom_api.node_type node with
  | 1 -> Some (Dom_api.unsafe_element node)
  | _ -> None

let make ~html ~holes =
  let root =
    lazy
      (let template = Dom_api.create_element Dom_api.document "template" in
       let rec stop index =
         if index >= String.length html || String.contains " />" html.[index] then index
         else stop (index + 1)
       in
       let tag = String.sub html 1 (max 0 (stop 1 - 1)) in
       let parent =
         match tag with
         | "circle" | "clipPath" | "defs" | "ellipse" | "feBlend" | "feColorMatrix"
         | "feComposite" | "feFlood" | "feGaussianBlur" | "feMerge" | "feMergeNode" | "feOffset"
         | "filter" | "foreignObject" | "g" | "image" | "line" | "linearGradient" | "marker"
         | "mask" | "path" | "pattern" | "polygon" | "polyline" | "radialGradient" | "rect"
         | "stop" | "symbol" | "text" | "textPath" | "tspan" | "use" ->
             Dom_api.set_inner_html template ("<svg>" ^ html ^ "</svg>");
             Dom_api.first_child (Dom_api.template_content template)
         | _ ->
             Dom_api.set_inner_html template html;
             Some (Dom_api.to_node (Dom_api.template_content template))
       in
       match Option.bind (Option.bind parent Dom_api.first_child) element_of with
       | Some root ->
           let rec unmark node =
             Option.iter unmark (Dom_api.first_child node);
             let next = Dom_api.next_sibling node in
             (match Dom_api.node_type node with
             | 8 ->
                 Option.iter
                   (fun parent ->
                     Dom_api.replace_child parent
                       (Dom_api.create_text_node Dom_api.document "")
                       node)
                   (Dom_api.parent_node node)
             | _ -> ());
             Option.iter unmark next
           in
           Option.iter unmark (Dom_api.first_child root);
           root
       | None -> Js.Exn.raiseError ("melhole: template needs a root element: " ^ html))
  in
  { html; holes; root }

let clone t =
  let root = Dom_api.clone_node (Lazy.force t.root) in
  let fail path =
    Js.Exn.raiseError
      ("melhole: template path ["
      ^ String.concat "; " (List.map string_of_int path)
      ^ "] does not resolve in " ^ t.html)
  in
  let rec slot candidate index =
    match (candidate, index) with
    | _, 0 -> Ok candidate
    | Some node, _ -> slot (Dom_api.next_sibling node) (index - 1)
    | None, _ -> Error ()
  in
  let rec descend element path =
    match path with
    | [] -> Some element
    | index :: rest -> (
        match slot (Dom_api.first_child element) index with
        | Ok (Some node) ->
            Option.bind (element_of node) (fun child -> descend child rest)
        | Ok None | Error () -> None)
  in
  let element path =
    match descend root path with Some element -> element | None -> fail path
  in
  let position (path, index) =
    let parent = element path in
    match slot (Dom_api.first_child parent) index with
    | Ok before -> { parent = Dom_api.to_node parent; before }
    | Error () -> fail (path @ [ index ])
  in
  ( root,
    Array.map
      (fun hole ->
        match hole with
        | Attr (path, name) -> Attr (element path, name)
        | Prop (path, name) -> Prop (element path, name)
        | Text at -> Text (position at)
        | Child at -> Child (position at)
        | Handler (path, name) -> Handler (element path, name)
        | Ref path -> Ref (element path)
        | Style path -> Style (element path)
        | Class_list path -> Class_list (element path))
      t.holes )
