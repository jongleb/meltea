(** Effects an update asks for: values describing them; the program runs them. *)

type 'msg t

val none : 'msg t
val batch : 'msg t list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val message : 'msg -> 'msg t
val perform : (unit -> 'value Js.Promise.t) -> ('value -> 'msg) -> 'msg t
val run : 'msg t -> ('msg -> unit) -> unit
