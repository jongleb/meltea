(** Views on a server and back in the browser: HTML in the format of React's [renderToString], and
    hydration that takes it over, in document order, instead of building it again. *)

val following : View.medium
val render : View.child -> string
val hydrate : View.child -> parent:'a Dom.node_like -> View.mounted

(** The JSON text the server wrote with [Melhole_server.data ~id], if the page has it. *)
val data : id:string -> string option
