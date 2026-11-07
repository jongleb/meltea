(** How an event reaches its handler. *)
type t =
  | Delegated
      (** One listener on [document] serves every element: the common bubbling events, and
          [onchange], React's [onChange] on an input or textarea, which listens to [input] and
          [change]. *)
  | Direct  (** A listener on the element itself. *)

val of_event : string -> t
