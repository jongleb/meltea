(** What a program listens to. A server renders a model and never listens. *)

type 'msg t

val none : 'msg t
val batch : 'msg t list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val every : int -> (float -> 'msg) -> 'msg t
val hash : (string -> 'msg) -> 'msg t
