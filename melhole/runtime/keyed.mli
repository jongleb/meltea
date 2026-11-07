(** Old keys of a keyed list by key: a JavaScript [Map], whose string hashing is native. *)
type t

val make : unit -> t

(** The entry recorded for a key, or -1. *)
val find : t -> string -> int

val set : t -> string -> int -> unit
