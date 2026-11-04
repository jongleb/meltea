(** What a program listens to while a model is current: values; the program starts and stops them. *)

type 'msg t

val none : 'msg t
val batch : 'msg t list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val every : int -> (float -> 'msg) -> 'msg t
val hash : (string -> 'msg) -> 'msg t

(** The subscriptions a program is running. *)
type 'msg active

val activate : unit -> 'msg active
val apply : 'msg active -> 'msg t -> ('msg -> unit) -> unit
