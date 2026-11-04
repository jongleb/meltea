external set_interval : (unit -> unit) -> int -> int = "setInterval"
external clear_interval : int -> unit = "clearInterval"
external now : unit -> float = "now" [@@mel.scope "Date"]

external add_listener : string -> (Dom.event -> unit) -> unit = "addEventListener"
[@@mel.scope "window"]

external remove_listener : string -> (Dom.event -> unit) -> unit = "removeEventListener"
[@@mel.scope "window"]

external hash_of_location : string = "hash" [@@mel.scope "window", "location"]

(** Where events come from, typed by the value each event carries. *)
type 'event source = Interval : int -> float source | Hash : string source

(** One thing to listen to, and the message each of its events becomes. *)
type 'msg subscription =
  | Subscription : { source : 'event source; tag : 'event -> 'msg } -> 'msg subscription

type 'msg t = Subscriptions of 'msg subscription list [@@unboxed]

(** Evidence that two sources carry events of the same type. *)
type ('a, 'b) same = Same : ('a, 'a) same

(** A listener that runs, found again by its source and its rank among listeners to that source. *)
type 'msg running =
  | Running : {
      source : 'event source;
      rank : int;
      tag : ('event -> 'msg) ref;
      stop : unit -> unit;
    }
      -> 'msg running

type 'msg active = { mutable running : 'msg running list; mutable sink : 'msg -> unit }

let none = Subscriptions []

let batch subscriptions =
  Subscriptions (List.concat_map (fun (Subscriptions list) -> list) subscriptions)

let map change (Subscriptions list) =
  Subscriptions
    (List.map
       (fun (Subscription { source; tag }) ->
         Subscription { source; tag = (fun event -> change (tag event)) })
       list)

let every interval tag = Subscriptions [ Subscription { source = Interval interval; tag } ]
let hash tag = Subscriptions [ Subscription { source = Hash; tag } ]
let activate () = { running = []; sink = ignore }

let same_source : type a b. a source -> b source -> (a, b) same option =
 fun first second ->
  match (first, second) with
  | Interval one, Interval other when one = other -> Some Same
  | Hash, Hash -> Some Same
  | Interval _, _ | Hash, _ -> None

let apply active (Subscriptions list) dispatch =
  active.sink <- dispatch;
  let wanted =
    List.mapi
      (fun index (Subscription { source; _ } as subscription) ->
        ( List.length
            (List.filteri
               (fun earlier (Subscription other) ->
                 earlier < index && Option.is_some (same_source source other.source))
               list),
          subscription ))
      list
  in
  let start : type event. event source -> int -> (event -> _) -> _ running =
   fun source rank tag ->
    let current = ref tag in
    let stop =
      match source with
      | Interval interval ->
          let handle = set_interval (fun () -> active.sink (!current (now ()))) interval in
          fun () -> clear_interval handle
      | Hash ->
          let listener _ = active.sink (!current hash_of_location) in
          add_listener "hashchange" listener;
          fun () -> remove_listener "hashchange" listener
    in
    Running { source; rank; tag = current; stop }
  in
  let renewed =
    List.map
      (fun (rank, Subscription { source; tag }) ->
        match
          List.find_map
            (fun (Running running as found) ->
              match (running.rank = rank, same_source source running.source) with
              | true, Some Same ->
                  running.tag := tag;
                  Some found
              | true, None | false, _ -> None)
            active.running
        with
        | Some found -> found
        | None -> start source rank tag)
      wanted
  in
  List.iter
    (fun (Running running) ->
      if
        not
          (List.exists
             (fun (rank, Subscription { source; _ }) ->
               rank = running.rank && Option.is_some (same_source source running.source))
             wanted)
      then running.stop ())
    active.running;
  active.running <- renewed
