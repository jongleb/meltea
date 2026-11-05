(** Views on a server and back in the browser: HTML with markers around every dynamic part, and
    hydration that takes over that HTML instead of building it again. *)

val render : View.child -> string
val hydrate : View.child -> parent:'a Dom.node_like -> View.mounted
