(** Effects an update asks for. A server renders a model and never runs them. *)

type 'msg t

val none : 'msg t
val batch : 'msg t list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val message : 'msg -> 'msg t
val perform : (unit -> 'promise) -> ('value -> 'msg) -> 'msg t
val focus : string -> 'msg t
val port : ('value -> unit) -> 'value -> 'msg t
