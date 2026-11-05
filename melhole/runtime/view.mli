(** Descriptions of DOM content, and the patcher that turns
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
  'item array -> ('item -> child[@u]) -> ('item -> input array[@u]) -> input array -> child
val tpl_instance : Template.t -> hole array -> child
val lazy1 : ('a -> child) -> 'a -> child
val lazy2 : ('a -> 'b -> child) -> 'a -> 'b -> child
val portal : Dom.element -> child -> child
val memo : input array -> (unit -> child) -> child
val pure : 'identity -> input array -> (unit -> child) -> child
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
val compare_inputs : input array -> input array -> change

(** What a library that mounts its own kind of child keeps in it, like Melact's components. *)
type hosted = ..

type state

(** A mounted child: its nodes and how to let go of what they hold. *)
type live = private {
  state : state;
  each : ((Dom.node -> unit[@u]) -> unit[@u]);  (** Visits the top-level nodes in document order. *)
  front : (Dom.node Js.null -> Dom.node Js.null[@u]);  (** The first node, or the given one. *)
  release : (unit -> unit[@u]);  (** Releases handlers, effects and portals; the nodes stay. *)
}

val nothing : live

(** Whether a content hole still has the server's markers around its place. *)
type range = Settled | Closing_at of Dom.node

(** How children reach the DOM: built in the browser, written by a server, or adopted from its HTML. *)
type medium = {
  text : (Template.position -> string option -> Dom.text[@u]);  (** [None] keeps a text already there. *)
  root : (Template.position -> Template.t -> Dom.element[@u]);
  placed : (Template.position -> Dom.element -> unit[@u]);
  opened : (Template.position -> range[@u]);  (** Where a content hole's children go. *)
  closed : (Dom.node -> Dom.node -> range[@u]);  (** After a hole's first children, before its closing marker. *)
  built : (int -> Template.position -> (int -> Template.position -> live[@u]) -> live array[@u]);
  aside : ((unit -> live[@u]) -> live[@u]);  (** Children that live elsewhere, like a portal's. *)
}

val browser : medium
val medium : medium ref
val backwards : int -> Template.position -> (int -> Template.position -> live[@u]) -> live array

val live :
  hosted ->
  each:((Dom.node -> unit[@u]) -> unit[@u]) ->
  front:(Dom.node Js.null -> Dom.node Js.null[@u]) ->
  release:(unit -> unit[@u]) ->
  live

val hosted : live -> hosted option

val host :
  build:(Template.position -> live[@u]) -> patch:(Template.position -> live -> live option[@u]) -> child

val build : child -> Template.position -> live
val update : Template.position -> live -> child -> live
val insert : Template.position -> Dom.node -> unit

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
