(** How to turn an old keyed sequence into a new one with few DOM moves. *)

(** What happens to one entry of the new sequence. *)
type step =
  | Keep of int  (** Reuse old entry [i]; it is already in order. *)
  | Move of int  (** Reuse old entry [i] and move it into place. *)
  | Create  (** Mount a new entry. *)

type t = {
  steps : step array;  (** One step per entry of the new sequence. *)
  dropped : int list;  (** Old entries that are not reused. *)
}

val plan : old:string array -> next:string array -> t
