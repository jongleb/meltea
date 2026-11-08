(** Program slices. A view is split into the [let]s its page reads and the statements it does not,
    and the page part becomes a function of the bits of the fields that changed: [Program] calls it
    instead of the view after every message, and each hole of a template in it is evaluated only when
    a field it reads changed, or a local the compiler cannot follow is not the one it was. *)

(** What the compiler found in one view: which of its [let]s the page needs and what every local at
    each template site reads. *)
type t

(** The plan for a top-level view, when its parameter and the [let]s before its page can be read.
    [closers] are the bits each message of the module's [update] writes: a hole that every message
    reaches keeps no gate. *)
val plan : closers:int list option -> Ppxlib.value_binding -> t option

(** How the template at a site of the planned view is narrowed; [locals] are the names local there. *)
val gates : t -> locals:string list -> Ppxlib.location -> Melhole_compiler.Element.gates

(** The compiled view split into its slice, hoisted, and the view that calls it with every bit; [None]
    when the compiled view no longer has the planned shape, and then it is given every bit itself. *)
val split : t -> Ppxlib.value_binding -> Ppxlib.value_binding * Ppxlib.expression option
