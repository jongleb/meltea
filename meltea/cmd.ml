external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"
external request_animation_frame : (float -> unit) -> int = "requestAnimationFrame"
external document : Dom.document = "document"

external element_by_id : Dom.document -> string -> Dom.element option = "getElementById"
[@@mel.send] [@@mel.return nullable]

external focus_element : Dom.element -> unit = "focus" [@@mel.send]

type 'msg t = Effects of (('msg -> unit) -> unit) list [@@unboxed]

(** How the next draw shows its changes: at once, or as a view transition of the browser. *)
type drawing = Instant | View_transition

let drawing = ref Instant

let none = Effects []
let batch commands = Effects (List.concat_map (fun (Effects jobs) -> jobs) commands)

let map tag (Effects jobs) =
  Effects (List.map (fun job dispatch -> job (fun message -> dispatch (tag message))) jobs)

let message value = Effects [ (fun dispatch -> queue_microtask (fun () -> dispatch value)) ]

let perform task tag =
  Effects
    [
      (fun dispatch ->
        ignore
          (Js.Promise.then_
             (fun value ->
               dispatch (tag value);
               Js.Promise.resolve ())
             (task ())));
    ]

let focus id =
  Effects
    [
      (fun _ ->
        ignore
          (request_animation_frame (fun _ ->
               Option.iter focus_element (element_by_id document id))));
    ]

let view_transition = Effects [ (fun _ -> drawing := View_transition) ]
let port send value = Effects [ (fun _ -> send value) ]
let run (Effects jobs) dispatch = List.iter (fun job -> job dispatch) jobs
