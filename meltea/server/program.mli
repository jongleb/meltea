(** Programs on a server: the first model's page as HTML for [Program.hydrate] to take over. *)

(** The model fields each message may change, as the compiler found them. *)
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

(** The model fields a view reads, as the compiler found them. *)
type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread

val mutable_read : unit
val render : view:('model -> 'msg Html.t) -> 'model -> string
