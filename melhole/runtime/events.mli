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

(** Listens at the document for a delegated event, once per event type, and gives the property an
    element keeps its handler for it under; compiled templates call it once, not per element. *)
val delegate : string -> string

(** Listens once on [document] for an element being entered or left ([mouseenter], [mouseleave],
    [pointerenter], [pointerleave]); compiled templates call it, so an app without such handlers
    carries none of it; gives the property an element keeps its handler for it under. *)
val delegate_crossing : string -> string

(** Records what an input or textarea shows now, its value or checked state, as React's value
    tracker does: [onchange], React's [onChange] on those, delivers an [input] or [change] event only
    when what the element shows differs from what was recorded. *)
val track : Dom.element -> unit

(** Runs after the handlers of an [input] or [change] event on a controlled field, before its value
    is put back: Melact renders there at once, as React does with [flushSync] for controlled inputs. *)
val after_controlled : (unit -> unit) ref

(** [segment event slot]: runs the delegated handlers for [event] from where the last segment
    stopped, or its target, up to [slot], which they do not reach, and makes the document's walk
    start at [slot]; an island of real React calls it from a slot's React handler, so that Melact's
    handlers between islands run inside React's propagation. *)
val segment : Dom.event -> Dom.node -> unit

(** [resume_at event node]: makes the document's walk start at [node]; an island of real React calls
    it as React's propagation leaves the island, so that the walk goes on from the island's place in
    Melact's tree, wherever the island's content was portaled. *)
val resume_at : Dom.event -> Dom.node -> unit

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
