type 'msg t = ('msg -> unit) -> Melhole_server.child
type event

let node view = view
let render view dispatch = view dispatch
let none _ = Melhole_server.empty
let text content _ = Melhole_server.text content
let int number _ = Melhole_server.text (string_of_int number)
let float number _ = Melhole_server.text (Melhole_server.float number)
let list views dispatch = Melhole_server.list (List.map (fun view -> view dispatch) views)
let keyed views dispatch = Melhole_server.list (List.map (fun (_, view) -> view dispatch) views)
let transition ~name:_ ?duration:_ view = view
let transition_group ~name:_ ?duration:_ view = view
let map tag view dispatch = view (fun message -> dispatch (tag message))
let memo view model = view model
let target_value _ = ""
let target_checked _ = false
let key _ = ""
let prevent_default _ = ()
