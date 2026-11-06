(** Views on a server without a DOM: HTML written straight into a buffer, in the format of React's
    [renderToString], so that [Served.hydrate] and React take it over. The same code runs natively
    and under Melange. *)

(** HTML being written: the last node written, the position React numbers [useId] by, and the
    value a [select] above chose. *)
type writer

(** Something that writes its HTML. *)
type child

val empty : child
val text : string -> child
val list : child list -> child

(** Children as a JavaScript array is rendered by React: each one at its own position, which
    React's [useId] numbers. *)
val array : child array -> child

val element : (writer -> unit) -> child
val write : writer -> child -> unit
val render : child -> string

(** The HTML without what only hydration needs, as React's [renderToStaticMarkup] writes it: no
    comment between two texts and none around Suspense boundaries. *)
val render_static : child -> string

(** The children a child holds: a list's or an array's items, none for [empty], else itself. *)
val items : child -> child list

(** The texts a child is made of, joined, as React reads an [option]'s value from its children;
    anything else counts for nothing. *)
val texts : child -> string

(** Writes the child at the positions given as count, index, count, index…: the index of each
    array it is in, among how many, from the outermost. *)
val forked : writer -> int array -> child -> unit

(** A position as React's [useId] numbers it: the index of each array it is in, among how many,
    from the root. *)
module Tree : sig
  type t

  val root : t

  (** [pushed tree count index]: item [index] of an array of [count] at [tree]. *)
  val pushed : t -> int -> int -> t

  (** [id tree local]: the [local]th [useId] of a component at [tree], as React spells it: [:R], the
      tree id in base 32, for the second and later ones [H] and [local] in base 32, and [:]. *)
  val id : t -> int -> string
end

(** The position being written. *)
val position : writer -> Tree.t

(** [recover child replacement]: [child], or if writing it raises, what it wrote taken back and
    [replacement] of the exception written in its place. *)
val recover : child -> (exn -> child) -> child

(** Markup known at compile time, already escaped, that ends with a tag. *)
val markup : writer -> string -> unit

(** Markup known at compile time, already escaped, that ends with a text. *)
val markup_text : writer -> string -> unit

(** Static text, already escaped, after a part whose last node is known only at run time. *)
val static_text : writer -> string -> unit

val dynamic_text : writer -> string -> unit

(** Markup that only hydration reads, like React's comments around a Suspense boundary: left out
    of static HTML. *)
val hydration_markup : writer -> string -> unit

(** The newline React writes first in a [textarea], [pre] or [listing] whose content starts with one. *)
val leading_newline : writer -> string -> unit

val attribute : writer -> string -> string option -> unit
val flag : writer -> string -> bool -> unit
val style : writer -> (string * string) list -> unit
val raw : writer -> string -> unit
val escape : string -> string

(** What the [select] being written chose, for the options below it. *)
type choice

(** [choose writer value] starts a [select] that chose [value], if any, and gives back the choice
    of the one around it, for [chosen]. *)
val choose : writer -> string option -> choice

val chosen : writer -> choice -> unit

(** Whether an [option] asks to be selected itself. *)
type marked = Marked | Unmarked

(** [selected=""] for an option of this value when its [select] chose it, or chose nothing and the
    option is [Marked]. *)
val option_selected : writer -> string -> marked -> unit

(** A float as JavaScript's [String(x)] writes it. *)
val float : float -> string

(** A script for the page's [<head>]: it records the events of the HTML before the code arrives,
    and the first hydration delivers those inside its root to their handlers, in order. *)
val capture : string

(** A [<script type="application/json">] holding JSON text, for the browser to read back with
    [Served.data]: every [<] is written as [<], so no text in the data can end the script. *)
val data : id:string -> string -> string
