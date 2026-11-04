open Melhole

type 'msg t = Node of (('msg -> unit) -> View.child) [@@unboxed]

external target : Dom.event -> Dom.element = "target" [@@mel.get]
external value : Dom.element -> string = "value" [@@mel.get]
external checked : Dom.element -> bool = "checked" [@@mel.get]
external event_key : Dom.event -> string = "key" [@@mel.get]
external prevent : Dom.event -> unit = "preventDefault" [@@mel.send]

let none = Node (fun _ -> View.empty)
let text content = Node (fun _ -> View.text content)
let int number = Node (fun _ -> View.text (string_of_int number))
let float number = Node (fun _ -> View.text (Js.Float.toString number))

let list views =
  Node (fun dispatch -> View.list (List.map (fun (Node render) -> render dispatch) views))

let keyed views =
  Node
    (fun dispatch ->
      View.keyed (List.map (fun (key, Node render) -> (key, render dispatch)) views))

let map tag (Node render) = Node (fun dispatch -> render (fun message -> dispatch (tag message)))

let memo view model =
  Node
    (fun dispatch ->
      View.memo [| View.Input view; View.Input model |] (fun () ->
          let (Node render) = view model in
          render dispatch))

let target_value event = value (target event)
let target_checked event = checked (target event)
let key event = event_key event
let prevent_default event = prevent event
