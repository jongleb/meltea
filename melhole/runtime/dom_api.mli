(** Raw DOM bindings used by the template cloner, the patcher and event
    delegation. *)

external document : Dom.document = "document"

external create_element : Dom.document -> string -> Dom.element
  = "createElement"
[@@mel.send]

external create_text_node : Dom.document -> string -> Dom.text
  = "createTextNode"
[@@mel.send]

external add_document_listener :
  Dom.document -> string -> (Dom.event -> unit) -> unit = "addEventListener"
[@@mel.send]

external add_listener : Dom.element -> string -> (Dom.event -> unit) -> unit
  = "addEventListener"
[@@mel.send]

external remove_listener :
  Dom.element -> string -> (Dom.event -> unit) -> unit = "removeEventListener"
[@@mel.send]

external to_node : 'a Dom.node_like -> Dom.node = "%identity"

external first_child : 'a Dom.node_like -> Dom.node option = "firstChild"
[@@mel.get] [@@mel.return nullable]

external next_sibling : 'a Dom.node_like -> Dom.node option = "nextSibling"
[@@mel.get] [@@mel.return nullable]

external parent_node : 'a Dom.node_like -> Dom.node option = "parentNode"
[@@mel.get] [@@mel.return nullable]

external node_type : Dom.node -> int = "nodeType" [@@mel.get]

external unsafe_element : Dom.node -> Dom.element = "%identity"

external insert_before :
  Dom.node -> 'a Dom.node_like -> Dom.node Js.null -> unit
  = "insertBefore"
[@@mel.send]

external remove_child : Dom.node -> 'a Dom.node_like -> unit = "removeChild"
[@@mel.send]

external replace_child : Dom.node -> 'a Dom.node_like -> 'b Dom.node_like -> unit
  = "replaceChild"
[@@mel.send]

external clone_node :
  Dom.element -> (_[@mel.as {json|true|json}]) -> Dom.element = "cloneNode"
[@@mel.send]

external set_inner_html : Dom.element -> string -> unit = "innerHTML"
[@@mel.set]

external template_content : Dom.element -> Dom.documentFragment = "content"
[@@mel.get]

external set_data : Dom.text -> string -> unit = "data" [@@mel.set]

external set_attribute : Dom.element -> string -> string -> unit
  = "setAttribute"
[@@mel.send]

external remove_attribute : Dom.element -> string -> unit = "removeAttribute"
[@@mel.send]

external last_child : Dom.node -> Dom.node Js.null = "lastChild" [@@mel.get]
external set_text_content : Dom.node -> string -> unit = "textContent" [@@mel.set]
external property : Dom.element -> string -> 'a = "" [@@mel.get_index]
external set_property : Dom.element -> string -> 'a -> unit = ""
[@@mel.set_index]

external style : Dom.element -> Dom.cssStyleDeclaration = "style" [@@mel.get]

external set_style : Dom.cssStyleDeclaration -> string -> string -> unit = ""
[@@mel.set_index]

external class_list : Dom.element -> Dom.domTokenList = "classList"
[@@mel.get]

external add_class : Dom.domTokenList -> string -> unit = "add" [@@mel.send]

external remove_class : Dom.domTokenList -> string -> unit = "remove"
[@@mel.send]

external set_handler : Dom.element -> string -> (Dom.event -> unit) -> unit
  = ""
[@@mel.set_index]

external handler : Dom.node -> string -> (Dom.event -> unit) option = ""
[@@mel.get_index] [@@mel.return undefined_to_opt]

external target : Dom.event -> Dom.node option = "target"
[@@mel.get] [@@mel.return nullable]

external cancel_bubble : Dom.event -> bool = "cancelBubble" [@@mel.get]

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

external request_animation_frame : (float -> unit) -> unit
  = "requestAnimationFrame"

external set_timeout : (unit -> unit) -> int -> unit = "setTimeout"
external compare_document_position : Dom.node -> Dom.node -> int = "compareDocumentPosition"
[@@mel.send]

external same : 'a -> 'b -> bool = "%eq"
(** Physical identity of two values of any types: [===] in JS. *)
