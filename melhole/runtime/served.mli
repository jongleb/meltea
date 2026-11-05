(** Views on a server and back in the browser: HTML in the format of React's [renderToString], and
    hydration that takes it over, in document order, instead of building it again. *)

val following : View.medium

(** A script for the page's [<head>]: it records the events of the HTML before the code arrives,
    and the first hydration delivers those inside its root to their handlers, in order. *)
val capture : string
val render : View.child -> string
val hydrate : View.child -> parent:'a Dom.node_like -> View.mounted
