(** A page whose root element is empty, cut where the app's HTML goes at build time. *)

type t

(** What becomes of the inputs the page gets before its code arrives. *)
type early =
  | Kept  (** The capture script goes in the head, and the first takeover replays them. *)
  | Lost

type error =
  | No_root of string  (** No element has this id. *)
  | Filled_root of string  (** The root element is not empty. *)
  | No_head  (** No [</head>] before the root, for the capture script. *)

val parse : root:string -> early -> string -> (t, [> `Shell of error ]) result

(** The page with the HTML right inside its root. *)
val fill : t -> string -> string
