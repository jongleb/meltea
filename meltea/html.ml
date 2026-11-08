open Melhole

type 'msg t = ('msg -> unit) -> View.child

external node : (('msg -> unit) -> View.child) -> 'msg t = "%identity"

external target : Dom.event -> Dom.element = "target" [@@mel.get]
external value : Dom.element -> string = "value" [@@mel.get]
external checked : Dom.element -> bool = "checked" [@@mel.get]
external event_key : Dom.event -> string = "key" [@@mel.get]
external prevent : Dom.event -> unit = "preventDefault" [@@mel.send]

let render view dispatch = view dispatch
let none _ = View.empty
let text content _ = View.text content
let int number _ = View.text (string_of_int number)
let float number _ = View.text (Js.Float.toString number)

let list views dispatch = View.list (List.map (fun view -> view dispatch) views)
let array views dispatch = View.list (Array.to_list (Array.map (fun view -> view dispatch) views))

let keyed views dispatch = View.keyed (List.map (fun (key, view) -> (key, view dispatch)) views)

let transition ~name ?duration view dispatch = Transition.make ~name ?duration (view dispatch)
let transition_group ~name ?duration view dispatch = Transition.group ~name ?duration (view dispatch)
let map tag view dispatch = view (fun message -> dispatch (tag message))

let memo view model dispatch =
  View.memo [| View.Input view; View.Input model |] (fun () -> view model dispatch)

let target_value event = value (target event)
let target_checked event = checked (target event)
let key event = event_key event
let prevent_default event = prevent event

let chunks ?(size = 100) ?(row_height = 40) ?(tag = "div") views dispatch =
  Melhole.View.chunks ~size ~row_height ~tag (Melhole.View.list (List.map (fun view -> view dispatch) views))
