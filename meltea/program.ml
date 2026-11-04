open Melhole

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = { send : 'msg -> unit; draw : unit -> unit; halt : unit -> unit }

(** Whether a redraw is waiting for the next microtask. *)
type schedule = Idle | Queued

(** What a kind of program does around the shared loop: apply a message, follow a drawn model, stop. *)
type ('model, 'msg) course = {
  step : 'msg -> 'model -> ('msg -> unit) -> 'model;
  drawn : 'model -> ('msg -> unit) -> unit;
  halted : ('msg -> unit) -> unit;
}

let start course ~view initial root =
  let model = ref initial and schedule = ref Idle and sink = ref ignore in
  let dispatch message = !sink message in
  let mounted =
    let (Html.Node render) = view initial in
    View.mount (render dispatch) ~parent:root ~before:None
  in
  let draw () =
    match !schedule with
    | Idle -> ()
    | Queued ->
        schedule := Idle;
        let (Html.Node render) = view !model in
        View.patch mounted (render dispatch);
        course.drawn !model dispatch
  in
  sink :=
    (fun message ->
      model := course.step message !model dispatch;
      match !schedule with
      | Queued -> ()
      | Idle ->
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

let element ~init ~update ~view ~subscriptions root =
  let initial, commands = init in
  let active = Sub.activate () in
  let program =
    start
      {
        step =
          (fun message model dispatch ->
            let next, commands = update message model in
            Cmd.run commands dispatch;
            next);
        drawn = (fun model dispatch -> Sub.apply active (subscriptions model) dispatch);
        halted = (fun dispatch -> Sub.apply active Sub.none dispatch);
      }
      ~view initial root
  in
  Sub.apply active (subscriptions initial) program.send;
  Cmd.run commands program.send;
  program

let sandbox ~init ~update ~view root =
  start
    { step = (fun message model _ -> update message model); drawn = (fun _ _ -> ()); halted = ignore }
    ~view init root

let dispatch program message = program.send message
let flush program = program.draw ()
let stop program = program.halt ()
