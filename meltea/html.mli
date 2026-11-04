(** Views: what to show for a model, and which messages its events send. Write them in mlx;
    meltea-ppx compiles each JSX tree to a template. *)

(** A view sending ['msg]: given the program's dispatch, the content to show. *)
type 'msg t = Node of (('msg -> unit) -> Melhole.View.child) [@@unboxed]

val none : 'msg t
val text : string -> 'msg t
val int : int -> 'msg t
val float : float -> 'msg t
val list : 'msg t list -> 'msg t
val keyed : (string * 'msg t) list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val memo : ('model -> 'msg t) -> 'model -> 'msg t
val target_value : Dom.event -> string
val target_checked : Dom.event -> bool
val key : Dom.event -> string
val prevent_default : Dom.event -> unit
