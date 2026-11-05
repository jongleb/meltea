(** Event handlers kept as a property of their element: delegated to [document] for the common
    bubbling events, attached directly otherwise. *)

(** What the element must give back when it goes. *)
type registration =
  | Delegated  (** Nothing: one [document] listener serves every element. *)
  | Direct of (unit -> unit)  (** Removes the element's own listener. *)

val bind : Dom.element -> string -> string * registration

val set : Dom.element -> string -> (Dom.event -> unit) -> unit

(** The property under which a controlled element keeps what puts its value back after input. *)
val controlled : string

val replay : Dom.node -> unit
