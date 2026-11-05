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
val each :
  'item list -> ('item -> child[@u]) -> ('item -> input array[@u]) -> input array -> child
val tpl_instance : Template.t -> hole array -> child
val lazy1 : ('a -> child) -> 'a -> child
val lazy2 : ('a -> 'b -> child) -> 'a -> 'b -> child
val component : Component.t -> input array -> (unit -> child) -> child
val provide : 'value Context.t -> 'value -> child -> child
val portal : Dom.element -> child -> child
val memo : input array -> (unit -> child) -> child
val items : child -> child list

(** A mounted compiled template: the values its refresh last read and what it holds. *)
type scope

(** A compiled template: its markup and how to make its holes on a fresh clone. *)
type 'holes template

(** Whether a value differs from the one a scope remembers for it. *)
type change = Changed | Unchanged

val template : Template.t -> (scope -> Dom.node -> 'holes[@u]) -> 'holes template
val compiled : 'holes template -> int -> (scope -> 'holes -> unit[@u]) -> child
val static : Template.t -> child
val check : scope -> int -> 'a -> change

(** The holes of a compiled template, written directly by its refresh. *)
module Hole : sig
  type text
  type attr
  type prop
  type style
  type handler
  type reference
  type content
end

val text_hole : Template.position -> Hole.text
val set_text : Hole.text -> string -> unit
val attr_hole : Dom.element -> string -> Hole.attr
val set_attr : Hole.attr -> string option -> unit
val prop_hole : Dom.element -> string -> Hole.prop
val set_prop : Hole.prop -> 'a -> unit
val controlled_hole : Dom.element -> string -> Hole.prop
val style_hole : Dom.element -> Hole.style
val set_style : Hole.style -> (string * string) list -> unit
val handler_hole : scope -> Dom.element -> string -> Hole.handler
val set_handler : Hole.handler -> (Dom.event -> unit) -> unit
val reference_hole : scope -> Dom.element -> Hole.reference
val set_reference : Hole.reference -> (Dom.element -> unit) -> unit
val content_hole : scope -> Template.position -> Hole.content
val set_content : Hole.content -> child -> unit

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
