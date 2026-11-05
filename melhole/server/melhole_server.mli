(** Views on a server without a DOM: HTML written straight into a buffer, in the format of React's
    [renderToString], so that [Served.hydrate] and React take it over. The same code runs natively
    and under Melange. *)

(** HTML being written, and whether the last node written was a text. *)
type writer

(** Something that writes its HTML. *)
type child

val empty : child
val text : string -> child
val list : child list -> child
val element : (writer -> unit) -> child
val write : writer -> child -> unit
val render : child -> string

(** Markup known at compile time, already escaped, that ends with a tag. *)
val markup : writer -> string -> unit

(** Markup known at compile time, already escaped, that ends with a text. *)
val markup_text : writer -> string -> unit

(** Static text, already escaped, after a part whose last node is known only at run time. *)
val static_text : writer -> string -> unit

val dynamic_text : writer -> string -> unit
val attribute : writer -> string -> string option -> unit
val flag : writer -> string -> bool -> unit
val style : writer -> (string * string) list -> unit
val raw : writer -> string -> unit
val escape : string -> string

(** A float as JavaScript's [String(x)] writes it. *)
val float : float -> string

(** A script for the page's [<head>]: it records the events of the HTML before the code arrives,
    and the first hydration delivers those inside its root to their handlers, in order. *)
val capture : string
