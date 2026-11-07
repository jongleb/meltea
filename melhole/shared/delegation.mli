(** How an event reaches its handler. *)
type t =
  | Delegated  (** One listener on [document] serves every element: the common bubbling events. *)
  | Direct  (** A listener on the element itself. *)

val of_event : string -> t
