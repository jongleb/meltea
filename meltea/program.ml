open Melhole

external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = { send : 'msg -> unit; draw : unit -> unit; halt : unit -> unit }
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread

(** The type declarations of the module that defines [update], as a digest. *)
type shape = Shape of string | Unshaped

type 'msg tracking = { changes : 'msg -> int; reads : int; shape : shape; listens : int }

external same : 'a -> 'b -> bool = "%eq"

(** When the next redraw is due: none, at the next microtask, or at the next frame. *)
type schedule = Idle | Queued | Framed

(** Which model a program starts from: its [init], or the model of the program running on its root. *)
type carry = Initial | Running

(** The view a program shows on its root, taken over when first asked for, and how it lets it go. *)
type page = { shown : unit -> View.mounted; gone : unit -> unit }

(** How a program takes over its root, and from which model. *)
type start = { take : (unit -> View.child) -> Dom.element -> page; carry : carry }

let held mounted = { shown = (fun () -> mounted); gone = (fun () -> View.unmount mounted) }
let mount = { take = (fun child root -> held (View.mount (child ()) ~parent:root ~before:None)); carry = Initial }
let hydrate = { take = (fun child root -> held (Served.hydrate (child ()) ~parent:root)); carry = Initial }

let resume =
  {
    take =
      (fun child root ->
        let page = Served.resumable child ~parent:root in
        { shown = (fun () -> Served.resumed page); gone = (fun () -> Served.leave page) });
    carry = Initial;
  }

let hot = { mount with carry = Running }

(** What a program leaves on its root for the next version of the code. *)
type 'model running = { built : shape; current : unit -> 'model; leave : unit -> unit }

let running_key = "$$meltea:program"

let carried ~start ~tracking root =
  match start.carry with
  | Initial -> None
  | Running ->
      Option.bind
        (Js.Undefined.toOption (Dom_api.property root running_key : 'model running Js.undefined))
        (fun previous ->
          let model = previous.current () in
          previous.leave ();
          match (previous.built, tracking.shape) with
          | Shape before, Shape now when String.equal before now -> Some model
          | (Shape _ | Unshaped), (Shape _ | Unshaped) -> None)

(** What a kind of program does around the shared loop: apply a message, follow a drawn model, stop. *)
type ('model, 'msg) course = {
  step : 'msg -> 'model -> ('msg -> unit) -> 'model;
  drawn : int -> 'model -> ('msg -> unit) -> unit;
  halted : ('msg -> unit) -> unit;
}

external start_view_transition : (unit -> unit) -> unit = "startViewTransition" [@@mel.scope "document"]
external view_transitions : (unit -> unit) Js.undefined = "startViewTransition" [@@mel.scope "document"]

let launch course ~tracking ~mode ~view initial root =
  let model = ref initial and schedule = ref Idle and sink = ref ignore and pending = ref 0 in
  let drawing = ref Cmd.Instant in
  let dispatch message = !sink message in
  let page = mode.take (fun () -> Html.render (view initial) dispatch) root in
  let patch () =
    let changed = !pending in
    if changed land tracking.reads <> 0 then View.patch (page.shown ()) (Html.render (view !model) dispatch);
    pending := 0;
    if changed land tracking.listens <> 0 then course.drawn changed !model dispatch
  in
  let draw () =
    match !schedule with
    | Idle -> ()
    | Queued | Framed -> (
        schedule := Idle;
        match (!drawing, Js.Undefined.toOption view_transitions) with
        | Cmd.View_transition, Some _ ->
            drawing := Cmd.Instant;
            start_view_transition patch
        | (Cmd.Instant | Cmd.View_transition), _ ->
            drawing := Cmd.Instant;
            patch ())
  in
  sink :=
    (fun message ->
      model := course.step message !model dispatch;
      (match !Cmd.drawing with
      | Cmd.View_transition ->
          drawing := Cmd.View_transition;
          Cmd.drawing := Cmd.Instant
      | Cmd.Instant -> ());
      pending := !pending lor tracking.changes message;
      match (!schedule, !Events.pace) with
      | Queued, _ | Framed, Continuous -> ()
      | Idle, Continuous ->
          schedule := Framed;
          Dom_api.request_animation_frame (fun _ -> draw ())
      | (Idle | Framed), Discrete ->
          schedule := Queued;
          queue_microtask draw);
  let halt () =
    sink := ignore;
    schedule := Idle;
    course.halted dispatch;
    page.gone ();
    Dom_api.set_property root running_key Js.undefined
  in
  Dom_api.set_property root running_key
    (Js.Undefined.return { built = tracking.shape; current = (fun () -> !model); leave = halt });
  { send = dispatch; draw; halt }

let everything = { changes = (fun _ -> -1); reads = -1; shape = Unshaped; listens = -1 }

let mutable_read = ()

let track ~update ~changes ~view ~reads ~shape ~subscriptions ~listens =
  match (changes, reads) with
  | Changes found, Reads read when same found.analysed update && same read.analysed view ->
      {
        changes = found.fields;
        reads = read.fields;
        shape;
        listens =
          (match listens with
          | Reads heard when same heard.analysed subscriptions -> heard.fields
          | Reads _ | Unread -> -1);
      }
  | (Changes _ | Unanalysed), (Reads _ | Unread) -> { everything with shape }

let element ?(tracking = everything) ?(start = mount) ~init ~update ~view ~subscriptions root =
  let initial, commands =
    Option.fold ~none:init ~some:(fun model -> (model, Cmd.none)) (carried ~start ~tracking root)
  in
  let active = Sub.activate () in
  let program =
    launch
      {
        step =
          (fun message model dispatch ->
            let next, commands = update message model in
            Cmd.run commands dispatch;
            next);
        drawn = (fun _ model dispatch -> Sub.apply active (subscriptions model) dispatch);
        halted = (fun dispatch -> Sub.apply active Sub.none dispatch);
      }
      ~tracking ~mode:start ~view initial root
  in
  Sub.apply active (subscriptions initial) program.send;
  Cmd.run commands program.send;
  program

let sandbox ?(tracking = everything) ?(start = mount) ~init ~update ~view root =
  launch
    { step = (fun message model _ -> update message model); drawn = (fun _ _ _ -> ()); halted = ignore }
    ~tracking ~mode:start ~view (Option.value ~default:init (carried ~start ~tracking root)) root

let render ~view model = Served.render (Html.render (view model) ignore)
let dispatch program message = program.send message
let flush program = program.draw ()
let stop program = program.halt ()
