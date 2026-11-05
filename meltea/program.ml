open Melhole

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = { send : 'msg -> unit; draw : unit -> unit; halt : unit -> unit }
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread
type 'msg tracking = { changes : 'msg -> int; reads : int }

external same : 'a -> 'b -> bool = "%eq"

(** When the next redraw is due: none, at the next microtask, or at the next frame. *)
type schedule = Idle | Queued | Framed

(** How a program takes over its root: empty, or holding the server's HTML of the first model. *)
type start = { take : View.child -> Dom.element -> View.mounted }

let mount = { take = (fun child root -> View.mount child ~parent:root ~before:None) }
let hydrate = { take = (fun child root -> Served.hydrate child ~parent:root) }

(** What a kind of program does around the shared loop: apply a message, follow a drawn model, stop. *)
type ('model, 'msg) course = {
  step : 'msg -> 'model -> ('msg -> unit) -> 'model;
  drawn : 'model -> ('msg -> unit) -> unit;
  halted : ('msg -> unit) -> unit;
}

let launch course ~tracking ~mode ~view initial root =
  let model = ref initial and schedule = ref Idle and sink = ref ignore and pending = ref 0 in
  let dispatch message = !sink message in
  let mounted =
    mode.take (Html.render (view initial) dispatch) root
  in
  let draw () =
    match !schedule with
    | Idle -> ()
    | Queued | Framed ->
        schedule := Idle;
        if !pending land tracking.reads <> 0 then View.patch mounted (Html.render (view !model) dispatch);
        pending := 0;
        course.drawn !model dispatch
  in
  sink :=
    (fun message ->
      model := course.step message !model dispatch;
      pending := !pending lor tracking.changes message;
      match (!schedule, !Events.pace) with
      | Queued, _ | Framed, Continuous -> ()
      | Idle, Continuous ->
          schedule := Framed;
          Dom_api.request_animation_frame (fun _ -> draw ())
      | (Idle | Framed), Discrete ->
          schedule := Queued;
          queue_microtask draw);
  {
    send = dispatch;
    draw;
    halt =
      (fun () ->
        course.halted dispatch;
        View.unmount mounted);
  }

let everything = { changes = (fun _ -> -1); reads = -1 }

let mutable_read = ()

let track ~update ~changes ~view ~reads =
  match (changes, reads) with
  | Changes found, Reads read when same found.analysed update && same read.analysed view ->
      { changes = found.fields; reads = read.fields }
  | (Changes _ | Unanalysed), (Reads _ | Unread) -> everything

let element ?(tracking = everything) ?(start = mount) ~init ~update ~view ~subscriptions root =
  let initial, commands = init in
  let active = Sub.activate () in
  let program =
    launch
      {
        step =
          (fun message model dispatch ->
            let next, commands = update message model in
            Cmd.run commands dispatch;
            next);
        drawn = (fun model dispatch -> Sub.apply active (subscriptions model) dispatch);
        halted = (fun dispatch -> Sub.apply active Sub.none dispatch);
      }
      ~tracking ~mode:start ~view initial root
  in
  Sub.apply active (subscriptions initial) program.send;
  Cmd.run commands program.send;
  program

let sandbox ?(tracking = everything) ?(start = mount) ~init ~update ~view root =
  launch
    { step = (fun message model _ -> update message model); drawn = (fun _ _ -> ()); halted = ignore }
    ~tracking ~mode:start ~view init root

let render ~view model = Served.render (Html.render (view model) ignore)
let dispatch program message = program.send message
let flush program = program.draw ()
let stop program = program.halt ()
