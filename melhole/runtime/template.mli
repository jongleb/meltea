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

(** The holes of one clone, found from its root. *)
type sites = Dom.node -> (Dom.element, position) hole array

type t

val html : string -> sites -> t
(** HTML whose root is an HTML element; the compiler generates the [sites] walk. *)

val svg : string -> sites -> t
(** HTML whose root is an SVG element, parsed inside [<svg>]. *)

val make : html:string -> holes:(int list, int list * int) hole array -> t
(** A hand-written HTML template whose holes are given by paths, found on every clone. *)

val clone : t -> Dom.element * (Dom.element, position) hole array

external first : Dom.node -> Dom.node = "firstChild" [@@mel.get]
external next : Dom.node -> Dom.node = "nextSibling" [@@mel.get]

external first_or_end : Dom.node -> Dom.node option = "firstChild"
[@@mel.get] [@@mel.return nullable]

external next_or_end : Dom.node -> Dom.node option = "nextSibling"
[@@mel.get] [@@mel.return nullable]

external element : Dom.node -> Dom.element = "%identity"
