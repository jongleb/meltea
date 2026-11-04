(** Typed hook slots of one component instance, read in call order. *)

(** The type witness of one hook call site, made once per site by melact-ppx. *)
type 'a key

val key : unit -> 'a key

(** The slots of one instance and the position of the next hook call. *)
type slots

val slots : unit -> slots
val rewind : slots -> unit
val take : slots -> 'a key -> (unit -> 'a) -> 'a
