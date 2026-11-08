(** How an event reaches its handler. *)
type t =
  | Delegated
      (** One listener on [document] serves every element: the common bubbling events, and
          [onchange], React's [onChange] on an input or textarea, which listens to [input] and
          [change]. *)
  | Crossing
      (** Entering and leaving an element: one listener on [document] for [mouseover] or
          [mouseout] works out which elements the pointer entered or left, as React does. *)
  | Direct  (** A listener on the element itself. *)

(** How an element built at run time listens: the common bubbling events are delegated. *)
val of_event : string -> t

(** How a compiled template listens: also the rarer bubbling events, which cost a running app
    nothing, since only templates that handle them name them. *)
val compiled : string -> t
