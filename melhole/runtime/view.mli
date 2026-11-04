(** Descriptions of DOM content and components, and the patcher that turns
    the mounted description into the next one by writing only what changed.
    Values are compared physically: mutating data in place is invisible. *)

type child

(** A prop value of any type, compared physically to decide whether a component re-renders. *)
type input = Input : 'a -> input [@@unboxed]

(** The value of one template hole. *)
type hole =
  | HText of string
  | HAttr of string option  (** [None] removes the attribute. *)
  | HProp : 'a -> hole  (** Written as a JS property ([value], [checked]). *)
  | HClass of string list  (** The classes that are on. *)
  | HStyle of (string * string) list  (** camelCase property, value. *)
  | HHandler of (Dom.event -> unit)  (** Replaced on every patch, never compared. *)
  | HRef of (Dom.element -> unit)  (** Called when mounted or replaced. *)
  | HChild of child

val text : string -> child
val empty : child
val list : child list -> child
val keyed : (string * child) list -> child
val with_key : string -> child -> child
val tpl_instance : Template.t -> hole array -> child
val lazy1 : ('a -> child) -> 'a -> child
val lazy2 : ('a -> 'b -> child) -> 'a -> 'b -> child
val component : Component.t -> input array -> (unit -> child) -> child
val provide : 'value Context.t -> 'value -> child -> child
val portal : Dom.element -> child -> child
val memo : input array -> (unit -> child) -> child
val items : child -> child list

(** A mounted template whose holes a compiled refresh fills. *)
type block

(** Whether a value differs from the one a block remembers for it. *)
type change = Changed | Unchanged

val guarded : Template.t -> int -> (block -> unit) -> child
val check : block -> int -> 'a -> change
val put_attr : block -> int -> string option -> unit
val put_prop : block -> int -> 'a -> unit
val put_text : block -> int -> string -> unit
val put_handler : block -> int -> (Dom.event -> unit) -> unit
val put_ref : block -> int -> (Dom.element -> unit) -> unit
val put_style : block -> int -> (string * string) list -> unit
val put_child : block -> int -> child -> unit

type mounted

val mount :
  child -> parent:'a Dom.node_like -> before:'b Dom.node_like option -> mounted

val patch : mounted -> child -> unit
val unmount : mounted -> unit

(** A mounted component, as seen by the hooks it calls. *)
type instance

(** When an effect runs after its instance is patched. *)
type timing =
  | Layout  (** Synchronously, before the browser paints. *)
  | Passive  (** After the whole flush, once the browser has painted. *)

val current : unit -> instance
val slot : instance -> 'a Hook.key -> (unit -> 'a) -> 'a
val invalidate : instance -> unit
(** Within one commit every cleanup runs before any effect, as in React. *)
type phase = Cleanup | Run

val after_commit : instance -> timing -> phase -> (unit -> unit) -> unit
val on_unmount : instance -> (unit -> unit) -> unit
val consume : instance -> 'value Context.t -> 'value
val flush : unit -> unit
