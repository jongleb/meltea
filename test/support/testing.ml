external describe : string -> (unit -> unit) -> unit = "describe"
external test : string -> (unit -> unit) -> unit = "test"

type 'a expectation

external expect : 'a -> 'a expectation = "expect"
external to_be : 'a expectation -> 'a -> unit = "toBe" [@@mel.send]
external to_equal : 'a expectation -> 'a -> unit = "toEqual" [@@mel.send]

external to_throw : (unit -> 'a) expectation -> string -> unit = "toThrow"
[@@mel.send]

type spy
type mock
type prototype

external element_prototype : prototype = "prototype" [@@mel.scope "Element"]
external node_prototype : prototype = "prototype" [@@mel.scope "Node"]

external event_target_prototype : prototype = "prototype"
[@@mel.scope "EventTarget"]
external spy_on : prototype -> string -> spy = "spyOn" [@@mel.scope "vi"]
external restore : spy -> unit = "mockRestore" [@@mel.send]
external mock : spy -> mock = "mock" [@@mel.get]
external calls : mock -> 'a array array = "calls" [@@mel.get]
external contexts : mock -> 'a array = "contexts" [@@mel.get]

external document : Dom.document = "document"
external body : Dom.document -> Dom.element = "body" [@@mel.get]

external create_element : Dom.document -> string -> Dom.element
  = "createElement"
[@@mel.send]

external append_child : Dom.element -> Dom.element -> unit = "appendChild"
[@@mel.send]

external inner_html : Dom.element -> string = "innerHTML" [@@mel.get]

external select : Dom.element -> string -> Dom.element = "querySelector"
[@@mel.send]

external click : Dom.element -> unit = "click" [@@mel.send]
external child_nodes : Dom.element -> Dom.nodeList = "childNodes" [@@mel.get]
external nodes : Dom.nodeList -> Dom.node array = "from" [@@mel.scope "Array"]
external text_content : 'a Dom.node_like -> string = "textContent" [@@mel.get]
external dom_property : Dom.element -> string -> 'a = "" [@@mel.get_index]
external style : Dom.element -> Dom.cssStyleDeclaration = "style" [@@mel.get]

external style_property : Dom.cssStyleDeclaration -> string -> string = ""
[@@mel.get_index]
external event : string -> Dom.event = "Event" [@@mel.new]
external dispatch : Dom.element -> Dom.event -> unit = "dispatchEvent" [@@mel.send]
external stop_propagation : Dom.event -> unit = "stopPropagation" [@@mel.send]
external same : 'a -> 'b -> bool = "%eq"

let attached () =
  let root = create_element document "div" in
  append_child (body document) root;
  root

let count_calls spy run =
  run ();
  let count = Array.length (calls (mock spy)) in
  restore spy;
  count

type 'a arbitrary
type property

external nat : int -> int arbitrary = "nat" [@@mel.module "fast-check"]

external array_of : 'a arbitrary -> < minLength : int ; maxLength : int > Js.t -> 'a array arbitrary
  = "array"
[@@mel.module "fast-check"]

external property : 'a arbitrary -> ('a -> unit) -> property = "property"
[@@mel.module "fast-check"]

external async_property : 'a arbitrary -> ('a -> unit Js.Promise.t) -> property = "asyncProperty"
[@@mel.module "fast-check"]

external check : property -> < numRuns : int > Js.t -> unit = "assert" [@@mel.module "fast-check"]

external check_async : property -> < numRuns : int > Js.t -> unit Js.Promise.t = "assert"
[@@mel.module "fast-check"]

(** A shrinkable sequence of choices, read left to right; past its end every choice is 0. *)
type tape = { values : int array; mutable at : int }

let tape values = { values; at = 0 }

let draw tape bound =
  if tape.at >= Array.length tape.values then 0
  else begin
    let value = tape.values.(tape.at) in
    tape.at <- tape.at + 1;
    value mod bound
  end

let tapes ~shortest ~longest =
  array_of (nat 1000) [%mel.obj { minLength = shortest; maxLength = longest }]

(** The DOM a user can observe: tags, attributes as a set, merged non-empty text. *)
type shape = Element_shape of string * (string * string) list * shape list | Text_shape of string

external node_type : Dom.node -> int = "nodeType" [@@mel.get]
external node_value : Dom.node -> string = "nodeValue" [@@mel.get]
external tag_name : Dom.node -> string = "tagName" [@@mel.get]
external attribute_names : Dom.node -> string array = "getAttributeNames" [@@mel.send]
external attribute : Dom.node -> string -> string = "getAttribute" [@@mel.send]
external node_children : Dom.node -> Dom.nodeList = "childNodes" [@@mel.get]
external as_node : Dom.element -> Dom.node = "%identity"

let rec shapes node =
  let merged =
    Array.fold_left
      (fun shown child ->
        match (node_type child, shown) with
        | 3, Text_shape previous :: rest -> Text_shape (previous ^ node_value child) :: rest
        | 3, _ -> Text_shape (node_value child) :: shown
        | 1, _ -> shape child :: shown
        | _, _ -> shown)
      [] (nodes (node_children node))
  in
  List.rev (List.filter (fun shown -> shown <> Text_shape "") merged)

and shape node =
  Element_shape
    ( tag_name node,
      List.sort compare
        (Array.to_list (Array.map (fun name -> (name, attribute node name)) (attribute_names node))),
      shapes node )

let rec without_empty_style shown =
  List.map
    (fun shape ->
      match shape with
      | Element_shape (tag, attributes, children) ->
          Element_shape
            ( tag,
              List.filter (fun (name, value) -> not (String.equal name "style" && String.equal value "")) attributes,
              without_empty_style children )
      | Text_shape _ -> shape)
    shown

let longest_increasing values =
  let values = Array.of_list values in
  let best = Array.make (Array.length values) 1 in
  Array.iteri
    (fun i value ->
      for k = 0 to i - 1 do
        if values.(k) < value then best.(i) <- max best.(i) (best.(k) + 1)
      done)
    values;
  Array.fold_left max 0 best

type observer

external observer : (unit -> unit) -> observer = "MutationObserver" [@@mel.new]

external observe :
  observer ->
  Dom.element ->
  < childList : bool ; subtree : bool ; attributes : bool ; characterData : bool > Js.t ->
  unit = "observe"
[@@mel.send]

external take_records : observer -> 'a array = "takeRecords" [@@mel.send]
external disconnect : observer -> unit = "disconnect" [@@mel.send]
