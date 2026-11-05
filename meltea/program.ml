open Melhole

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = { send : 'msg -> unit; draw : unit -> unit; halt : unit -> unit }
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread
type 'msg tracking = { changes : 'msg -> int; reads : int }

external same : 'a -> 'b -> bool = "%eq"

(** Whether a redraw is waiting for the next microtask. *)
type schedule = Idle | Queued

(** What a kind of program does around the shared loop: apply a message, follow a drawn model, stop. *)
type ('model, 'msg) course = {
  step : 'msg -> 'model -> ('msg -> unit) -> 'model;
  drawn : 'model -> ('msg -> unit) -> unit;
  halted : ('msg -> unit) -> unit;
}

let start course ~tracking ~view initial root =
  let model = ref initial and schedule = ref Idle and sink = ref ignore and pending = ref 0 in
  let dispatch message = !sink message in
  let mounted =
    View.mount (Html.render (view initial) dispatch) ~parent:root ~before:None
  in
  let draw () =
    match !schedule with
    | Idle -> ()
    | Queued ->
        schedule := Idle;
        if !pending land tracking.reads <> 0 then View.patch mounted (Html.render (view !model) dispatch);
        pending := 0;
        course.drawn !model dispatch
  in
  sink :=
    (fun message ->
      model := course.step message !model dispatch;
      pending := !pending lor tracking.changes message;
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

let everything = { changes = (fun _ -> -1); reads = -1 }

let track ~update ~changes ~view ~reads =
  match (changes, reads) with
  | Changes found, Reads read when same found.analysed update && same read.analysed view ->
      { changes = found.fields; reads = read.fields }
  | (Changes _ | Unanalysed), (Reads _ | Unread) -> everything

let element ?(tracking = everything) ~init ~update ~view ~subscriptions root =
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
      ~tracking ~view initial root
  in
  Sub.apply active (subscriptions initial) program.send;
  Cmd.run commands program.send;
  program

let sandbox ?(tracking = everything) ~init ~update ~view root =
  start
    { step = (fun message model _ -> update message model); drawn = (fun _ _ -> ()); halted = ignore }
    ~tracking ~view init root

let dispatch program message = program.send message
let flush program = program.draw ()
let stop program = program.halt ()
