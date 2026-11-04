external queue_microtask : (unit -> unit) -> unit = "queueMicrotask"

type 'msg t = Effects of (('msg -> unit) -> unit) list [@@unboxed]

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

let run (Effects jobs) dispatch = List.iter (fun job -> job dispatch) jobs
