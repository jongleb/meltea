(** How to turn an old keyed sequence into a new one with few DOM moves. *)

(** A row's key: a string, or an int kept as it is, so that JavaScript makes no string of it and
    fills no cache of number strings. Keys are the same when they are [===]; the keys of one loop
    are all of one kind. *)
type key

external string_key : string -> key = "%identity"
external int_key : int -> key = "%identity"
external string_keys : string array -> key array = "%identity"

(** What happens to one entry of the new sequence; an int inside, so that a plan allocates nothing
    per entry. *)
type step

(** A step as a variant, to match on where an allocation does not matter. *)
type view =
  | Keep of int  (** Reuse old entry [i]; it is already in order. *)
  | Move of int  (** Reuse old entry [i] and move it into place. *)
  | Create  (** Mount a new entry. *)

val view : step -> view

(** The old entry a step keeps in place, if it does. *)
val kept : step -> int option

(** The old entry a step moves into place, if it does. *)
val moved : step -> int option

type t = {
  steps : step array;  (** One step per entry of the new sequence. *)
  dropped : int array;  (** Old entries that are not reused, in order. *)
}

val plan : old:key array -> next:key array -> t
