(** Views: what to show for a model, and which messages its events send. Write them in mlx;
    meltea-ppx compiles each JSX tree to a template. *)

(** A view sending ['msg]: given the program's dispatch, the content to show. *)
type 'msg t

external node : (('msg -> unit) -> Melhole.View.child) -> 'msg t = "%identity"
val render : 'msg t -> ('msg -> unit) -> Melhole.View.child

val none : 'msg t
val text : string -> 'msg t
val int : int -> 'msg t
val float : float -> 'msg t
val list : 'msg t list -> 'msg t

(** [array (Array.map row items)] in a view compiles to the same keyed loop as [list (List.map row items)],
    reading the array as it is. *)
val array : 'msg t array -> 'msg t
val keyed : (string * 'msg t) list -> 'msg t

(** A long list cut into groups of [size] rows (default 100), each a [tag] element (default
    ["div"]) with [content-visibility: auto] and a height estimate from [row_height] pixels
    (default 40): the browser skips style, layout and paint of the groups out of view. The groups
    change the DOM, so selectors such as [:nth-child] see them; a row that moves to another group is
    made again. *)
val chunks : ?size:int -> ?row_height:int -> ?tag:string -> 'msg t list -> 'msg t
val transition : name:string -> ?duration:int -> 'msg t -> 'msg t
val transition_group : name:string -> ?duration:int -> 'msg t -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val memo : ('model -> 'msg t) -> 'model -> 'msg t
val target_value : Dom.event -> string
val target_checked : Dom.event -> bool
val key : Dom.event -> string
val prevent_default : Dom.event -> unit
