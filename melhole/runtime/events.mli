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

(** The property an element keeps its handler for an event under. *)
val key_of : string -> string

(** Listens for an event on behalf of an element: at the document once per event type when it
    bubbles, on the element otherwise. *)
val register : Dom.element -> string -> registration

(** Listens at the document for a delegated event, once per event type; compiled templates call it
    once, not per element. *)
val delegate : string -> unit

val listen : Dom.element -> string -> unit

val unlisten : Dom.element -> string -> unit

val set : Dom.element -> string -> (Dom.event -> unit) -> unit

(** The property under which a controlled element keeps what puts its value back after input. *)
val controlled : string

(** Events the capture script kept inside a root before the code arrived, each with the value and
    checked state its target had then. *)
type early

(** Takes the kept events from the capture script, which stops it; [None] when none is inside the root. *)
val early : Dom.node -> early option

(** Runs the function, then gives the event's target back the value and checked state it had. *)
val preserving : Dom.event -> (unit -> unit) -> unit

(** Delivers the kept events in order, each with its target's value of that moment. *)
val replay : early -> unit
