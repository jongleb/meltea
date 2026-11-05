(** A lowercase JSX tree turned into template HTML and the holes in it. *)

(** What a dynamic part of an element fills. *)
type site =
  | Attr of string
  | Prop of string
  | Control of string  (** A property the user changes too; put back after input. *)
  | Handler of string  (** The DOM event name. *)
  | Ref
  | Style
  | Sole_text  (** The element's only child, a dynamic text: written through the element, no anchor. *)

(** What a child slot inserts. *)
type slot = Text_slot | Child_slot

(** A static attribute. *)
type static = Valued of string | Bare

type 'value node =
  | Element of 'value element
  | Text of string  (** Static text. *)
  | Slot of slot * 'value

and 'value element = {
  tag : string;
  statics : (string * static) list;
  dynamics : (site * 'value) list;
  children : 'value node list;
}

type 'value hole =
  | On_element of int list * site * 'value  (** The element's path. *)
  | At_position of int list * int * slot * 'value
      (** The parent's path and the index of the node it goes before. *)

(** One step of taking over a server's nodes, in document order. *)
type 'value adoption =
  | Enter of int list * string  (** The element at this path; its children follow, then [Leave]. *)
  | Leave of int list
  | Static of int list * string  (** Static text under the element at this path. *)
  | Anchor of int list  (** A node the template keeps between neighbours. *)
  | Slot_at of int list * slot * 'value

val void : string -> [ `Void | `Container ]
val adoption : 'value element -> 'value adoption list
val render : 'value element -> string * 'value hole list

(** How generated code reaches a node of a clone. *)
type step =
  | First_of of int list  (** The first child of the node at that path. *)
  | Next_of of int list  (** The next sibling of the node at that path. *)

(** Where an inserted node goes, given the parent's path. *)
type anchor =
  | First_or_end of int list  (** Before the parent's first child, or appended. *)
  | Next_or_end of int list  (** Before the next sibling of that node, or appended. *)

val walk : 'value hole list -> (int list * step) list
(** Every node the holes need below the root, each after the node it is reached from. *)

val anchor : int list -> int -> anchor
