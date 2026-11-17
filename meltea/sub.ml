external set_interval : (unit -> unit) -> int -> int = "setInterval"
external clear_interval : int -> unit = "clearInterval"
external now : unit -> float = "now" [@@mel.scope "Date"]

external add_listener : string -> (Dom.event -> unit) -> unit = "addEventListener"
[@@mel.scope "window"]

external remove_listener : string -> (Dom.event -> unit) -> unit = "removeEventListener"
[@@mel.scope "window"]

external hash_of_location : string = "hash" [@@mel.scope "window", "location"]
external key_of : Dom.event -> string = "key" [@@mel.get]

(** Where events come from, typed by the value each event carries. *)
type 'event source = Interval : int -> float source | Hash : string source | Key_down : string source

(** One thing to listen to, how to start listening (giving back how to stop), and the message each
    of its events becomes. *)
type 'msg subscription =
  | Subscription : {
      source : 'event source;
      listen : ('event -> unit) -> unit -> unit;
      tag : 'event -> 'msg;
    }
      -> 'msg subscription

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
       (fun (Subscription { source; listen; tag }) ->
         Subscription { source; listen; tag = (fun event -> change (tag event)) })
       list)

let every interval tag =
  Subscriptions
    [
      Subscription
        {
          source = Interval interval;
          listen =
            (fun deliver ->
              let handle = set_interval (fun () -> deliver (now ())) interval in
              fun () -> clear_interval handle);
          tag;
        };
    ]

let hash tag =
  Subscriptions
    [
      Subscription
        {
          source = Hash;
          listen =
            (fun deliver ->
              let listener _ = deliver hash_of_location in
              add_listener "hashchange" listener;
              fun () -> remove_listener "hashchange" listener);
          tag;
        };
    ]

let key_down tag =
  Subscriptions
    [
      Subscription
        {
          source = Key_down;
          listen =
            (fun deliver ->
              let listener event = deliver (key_of event) in
              add_listener "keydown" listener;
              fun () -> remove_listener "keydown" listener);
          tag;
        };
    ]
let activate () = { running = []; sink = ignore }

let same_source : type a b. a source -> b source -> (a, b) same option =
 fun first second ->
  match (first, second) with
  | Interval one, Interval other when one = other -> Some Same
  | Hash, Hash -> Some Same
  | Key_down, Key_down -> Some Same
  | Interval _, _ | Hash, _ | Key_down, _ -> None

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
  let start source listen rank tag =
    let current = ref tag in
    Running { source; rank; tag = current; stop = listen (fun event -> active.sink (!current event)) }
  in
  let renewed =
    List.map
      (fun (rank, Subscription { source; listen; tag }) ->
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
        | None -> start source listen rank tag)
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
