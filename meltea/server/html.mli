(** Views on a server: the same API as in the browser, writing HTML instead of building the DOM.
    Compile the view's module with [meltea-ppx -server] against this library. *)

(** A view sending ['msg]; on a server it only writes. *)
type 'msg t

(** An event, which a server never sees. *)
type event

val node : (('msg -> unit) -> Melhole_server.child) -> 'msg t
val render : 'msg t -> ('msg -> unit) -> Melhole_server.child
val none : 'msg t
val text : string -> 'msg t
val int : int -> 'msg t
val float : float -> 'msg t
val list : 'msg t list -> 'msg t
val keyed : (string * 'msg t) list -> 'msg t
val transition : name:string -> ?duration:int -> 'msg t -> 'msg t
val transition_group : name:string -> ?duration:int -> 'msg t -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val memo : ('model -> 'msg t) -> 'model -> 'msg t
val target_value : event -> string
val target_checked : event -> bool
val key : event -> string
val prevent_default : event -> unit
