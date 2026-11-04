(** A static DOM fragment with holes, cloned per instance; HTML comments become empty text anchors. *)

(** Paths are child indices; a position is a parent path and the index it precedes. *)
type ('element, 'position) hole =
  | Attr of 'element * string  (** Attribute [name] of the element. *)
  | Prop of 'element * string  (** JS property [name] of the element. *)
  | Text of 'position  (** A text node inserted at the position. *)
  | Child of 'position  (** Child content inserted at the position. *)
  | Handler of 'element * string  (** Handler for event [name] (["click"]). *)
  | Ref of 'element  (** Callback receiving the element when mounted. *)
  | Style of 'element  (** Inline style properties of the element. *)
  | Class_list of 'element  (** Classes switched on in the element. *)

(** Where inserted content goes: before [before], or appended to [parent]. *)
type position = { parent : Dom.node; before : Dom.node option }

type t

val html : string -> t
(** Markup whose root is an HTML element. *)

val svg : string -> t
(** Markup whose root is an SVG element, parsed inside [<svg>]. *)

val make : html:string -> holes:(int list, int list * int) hole array -> t
(** A hand-written template whose holes are given by paths. *)

val clone : t -> Dom.element

val locate : t -> Dom.node -> (Dom.element, position) hole array
(** The holes of a hand-written template in a clone, found by their paths. *)

external first : Dom.node -> Dom.node = "firstChild" [@@mel.get]
external next : Dom.node -> Dom.node = "nextSibling" [@@mel.get]

external first_or_end : Dom.node -> Dom.node option = "firstChild"
[@@mel.get] [@@mel.return nullable]

external next_or_end : Dom.node -> Dom.node option = "nextSibling"
[@@mel.get] [@@mel.return nullable]

external element : Dom.node -> Dom.element = "%identity"
