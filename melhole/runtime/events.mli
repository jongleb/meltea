(** Event handlers kept as a property of their element: delegated to [document] for the common
    bubbling events, attached directly otherwise. *)

(** What the element must give back when it goes. *)
type registration =
  | Delegated  (** Nothing: one [document] listener serves every element. *)
  | Direct of (unit -> unit)  (** Removes the element's own listener. *)

(** How often a kind of event fires: once per action, or many times a frame (pointer moves, scroll). *)
type pace = Discrete | Continuous

(** The pace of the event whose handlers are running; [Discrete] outside handlers. *)
val pace : pace ref

val bind : Dom.element -> string -> string * registration

val set : Dom.element -> string -> (Dom.event -> unit) -> unit

(** The property under which a controlled element keeps what puts its value back after input. *)
val controlled : string

val replay : Dom.node -> unit
