open Melhole

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = { send : 'msg -> unit; draw : unit -> unit; halt : unit -> unit }

(** Whether a redraw is waiting for the next microtask. *)
type schedule = Idle | Queued

let element ~init ~update ~view ~subscriptions root =
  let initial, commands = init in
  let model = ref initial and schedule = ref Idle in
  let active = Sub.activate () in
  let rec send message =
    let next, commands = update message !model in
    model := next;
    Cmd.run commands send;
    match !schedule with
    | Queued -> ()
    | Idle ->
        schedule := Queued;
        queue_microtask draw
  and draw () =
    match !schedule with
    | Idle -> ()
    | Queued ->
        schedule := Idle;
        let (Html.Node render) = view !model in
        View.patch (Lazy.force mounted) (render send);
        Sub.apply active (subscriptions !model) send
  and mounted =
    lazy
      (let (Html.Node render) = view initial in
       View.mount (render send) ~parent:root ~before:None)
  in
  ignore (Lazy.force mounted);
  Sub.apply active (subscriptions initial) send;
  Cmd.run commands send;
  {
    send;
    draw;
    halt =
      (fun () ->
        Sub.apply active Sub.none send;
        View.unmount (Lazy.force mounted));
  }

let sandbox ~init ~update ~view root =
  element ~init:(init, Cmd.none)
    ~update:(fun message model -> (update message model, Cmd.none))
    ~view
    ~subscriptions:(fun _ -> Sub.none)
    root

let dispatch program message = program.send message
let flush program = program.draw ()
let stop program = program.halt ()
