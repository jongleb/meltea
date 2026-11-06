(** Views on a server and back in the browser: HTML in the format of React's [renderToString], and
    hydration that takes it over, in document order, instead of building it again. *)

val following : View.medium
val render : View.child -> string
val hydrate : View.child -> parent:'a Dom.node_like -> View.mounted

(** A server's page left as the server wrote it, without running its view, until the first input
    inside it or until [resumed] asks for it. *)
type resumable

(** Listens on [window], in the capture phase, for the first click, double click, input, change,
    key, submit or pointer press inside [parent]; then takes the page over as [hydrate] does and lets
    that input go on to its handlers, its target's value and checked state as the user left them. When
    the capture script kept events inside [parent] before the code arrived, the page is taken over at
    the next microtask instead and the events are replayed, as [hydrate] does. *)
val resumable : (unit -> View.child) -> parent:'a Dom.node_like -> resumable

(** The page taken over, now if nothing has taken it over yet. *)
val resumed : resumable -> View.mounted

(** Stops listening and empties the page, or unmounts it once taken over. *)
val leave : resumable -> unit

(** The JSON text the server wrote with [Melhole_server.data ~id], if the page has it. *)
val data : id:string -> string option
