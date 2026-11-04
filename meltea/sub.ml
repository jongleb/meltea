external set_interval : (unit -> unit) -> int -> int = "setInterval"
external clear_interval : int -> unit = "clearInterval"
external now : unit -> float = "now" [@@mel.scope "Date"]

(** One thing to listen to. *)
type 'msg subscription = Every of { interval : int; tag : float -> 'msg }

type 'msg t = Subscriptions of 'msg subscription list [@@unboxed]

(** A running timer, found again by its interval and its rank among timers of that interval. *)
type 'msg timer = { interval : int; rank : int; tag : (float -> 'msg) ref; handle : int }

type 'msg active = { mutable timers : 'msg timer list; mutable sink : 'msg -> unit }

let none = Subscriptions []

let batch subscriptions =
  Subscriptions (List.concat_map (fun (Subscriptions list) -> list) subscriptions)

let map change (Subscriptions list) =
  Subscriptions
    (List.map
       (fun (Every { interval; tag }) -> Every { interval; tag = (fun time -> change (tag time)) })
       list)

let every interval tag = Subscriptions [ Every { interval; tag } ]
let activate () = { timers = []; sink = ignore }

let apply active (Subscriptions list) dispatch =
  active.sink <- dispatch;
  let wanted =
    List.mapi
      (fun index (Every { interval; tag }) ->
        let rank =
          List.length
            (List.filteri
               (fun earlier (Every other) -> earlier < index && other.interval = interval)
               list)
        in
        (interval, rank, tag))
      list
  in
  List.iter
    (fun timer ->
      match
        List.find_opt
          (fun (interval, rank, _) -> interval = timer.interval && rank = timer.rank)
          wanted
      with
      | Some _ -> ()
      | None -> clear_interval timer.handle)
    active.timers;
  active.timers <-
    List.map
      (fun (interval, rank, tag) ->
        match
          List.find_opt (fun timer -> timer.interval = interval && timer.rank = rank) active.timers
        with
        | Some timer ->
            timer.tag := tag;
            timer
        | None ->
            let current = ref tag in
            {
              interval;
              rank;
              tag = current;
              handle = set_interval (fun () -> active.sink (!current (now ()))) interval;
            })
      wanted
