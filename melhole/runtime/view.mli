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
  | HRef of (Dom.element Js.nullable -> unit)  (** Called with the element when mounted, with null when it goes; a new one replaces the old, called with null first. *)
  | HChild of child

val text : string -> child
val empty : child
val list : child list -> child
val keyed : (string * child) list -> child
val with_key : string -> child -> child
(** Whether a compiled loop's rows are drawn from the item alone, or from the item and its index. *)
type numbering = Items | Numbered

(** How a selector compares the key it chose with a row's: OCaml's [=] or [==]. *)
type equality = Structural | Physical

(** A compiled loop site that has selectors, made once per site. *)
type selection

(** [selection others]: the bits of [others] are the outer inputs that the pieces other than selectors read. *)
val selection : int -> selection

(** A projected piece of a loop's row that is one value in the rows whose key is the one the outer
    values chose, and another in the rest. *)
type 'item selector

(** [selector equality chosen key]: [chosen] is the key the outer values choose, if any; [key] gives a
    row's key from its item and index. *)
val selector : equality -> 'key option -> ('item -> int -> 'key[@u]) -> 'item selector

(** The selectors of one render of a compiled loop, in the order of the pieces they are. When only
    inputs that selectors alone read change and the items stay, a loop draws again only the rows
    whose key a selector chose before or chooses now, looked up in an index of the rows by key. *)
type 'item selectors = Unselecting | Selecting of selection * 'item selector array

(** A compiled loop: [project item index out at] writes the [pieces] values of a row's rendering that read
    [outer] into [out] from [at], so that a change of [outer] re-renders only the rows they differ in
    and allocates nothing per row; a [Numbered] row is drawn again when its index changes. *)
(** One loop in the generated code, made once per loop site: a loop patches only the rows its own site drew. *)
type loop_site

val loop_site : unit -> loop_site

val each :
  loop_site ->
  numbering ->
  'source ->
  ('source -> 'item array) ->
  ('item -> int -> child[@u]) ->
  ('item -> int -> input array -> int -> unit[@u]) ->
  int ->
  input array ->
  'item selectors ->
  child

(** What a compiled template's frame needs at the moments only the runtime sees; [None] for nothing. *)
type 'frame duties = {
  placed : ('frame -> unit[@u]) option;  (** Once its root is in the document: refs get the element. *)
  released : ('frame -> unit[@u]) option;  (** When it goes: content released, listeners and refs let go. *)
  forgotten : ('frame -> unit[@u]) option;  (** After a render that threw halfway: every hole is written again. *)
}

(** A compiled template: its markup and how to make the frame of a fresh clone, the one record
    that holds a mounted clone's holes and the values its refresh last read. *)
type 'frame template

(** A compiled loop whose rows are one element each: one template, the root of a row's frame, one
    refresh taking the item, and the key of an item, shared by all rows; a row costs its nodes and
    its frame. *)
(** The items of a loop over an array: the array itself. *)
val arrayed : 'item array -> 'item array

val rows :
  'source ->
  ('source -> 'item array) ->
  'frame template ->
  ('frame -> Dom.element[@u]) ->
  ('frame -> 'item -> unit[@u]) ->
  ('item -> string[@u]) ->
  ('item -> int -> input array -> int -> unit[@u]) ->
  int ->
  input array ->
  'item selectors ->
  child
val tpl_instance : Template.t -> hole array -> child
val lazy1 : ('a -> child) -> 'a -> child
val lazy2 : ('a -> 'b -> child) -> 'a -> 'b -> child
val portal : Dom.element -> child -> child
val memo : input array -> (unit -> child) -> child
val pure : 'identity -> input array -> (unit -> child) -> child
val items : child -> child list

(** Whether a value differs from the one a scope remembers for it. *)
type change = Changed | Unchanged

val template : Template.t -> (Dom.node -> 'frame[@u]) -> 'frame duties -> 'frame template

val hydratable :
  Template.t -> (Dom.node -> 'frame[@u]) -> 'frame duties -> (Dom.node -> 'frame -> 'frame[@u]) -> 'frame template

val compiled : 'frame template -> ('frame -> unit[@u]) -> child
val static : Template.t -> child

val unset : input
val no_inputs : input array

val check : input -> 'a -> change

val compare_inputs : input array -> input array -> change

(** A mounted child: its nodes and what they hold. *)
type live

val nothing : live

val visit : live -> (Dom.node -> unit[@u]) -> unit

val front : live -> Dom.node Js.null -> Dom.node Js.null

val release : live -> unit

(** How a library's own kind of child, like Melact's components, reaches what it holds: made once per kind. *)
type 'value hosting

val hosting :
  visit:('value -> (Dom.node -> unit[@u]) -> unit[@u]) ->
  first:('value -> Dom.node Js.null -> Dom.node Js.null[@u]) ->
  let_go:('value -> unit[@u]) ->
  'value hosting

val live : 'value hosting -> 'value -> live

val hosted : 'value hosting -> live -> 'value option

(** Whether a content hole shows its children, or only remembers them for hydration's first pass. *)
type range = Settled | Collecting of collecting

(** The children a collecting content hole was last given. *)
and collecting

(** Where a medium's nodes come from. *)
type adoption = Fresh_nodes | Server_nodes

(** How children reach the DOM: built in the browser, or adopted from the server's HTML. *)
type medium = {
  adoption : adoption;
  text : (Template.position -> string option -> Dom.text[@u]);  (** [None]: the text is written later. *)
  root : (Template.position -> Template.t -> Dom.element[@u]);
  placed : (Template.position -> Dom.element -> unit[@u]);
  opened : (Template.position -> range[@u]);  (** What a new content hole does with its children. *)
  built :
    'made.
    (int ->
    Template.position ->
    (int -> Template.position -> 'made[@u]) ->
    ('made -> Dom.node Js.null -> Dom.node Js.null[@u]) ->
    'made array[@u]);
      (** Makes children in order; the last argument gives the node after one, from where it started. *)
  aside : ((unit -> live[@u]) -> live[@u]);  (** Children that live elsewhere, like a portal's. *)
}

val browser : medium
val medium : medium ref

(** What hydration tells about the positions it adopts children at, item [index] of [count]: Melact
    counts them as React's [useId] does; by default nothing does. [join] leaves the last [fork]. *)
type positions = { fork : (int -> int -> unit[@u]); join : (unit -> unit[@u]) }

val positions : positions ref

(** In what order the patch under way reaches the children it patches: [Out_of_order] while a list
    patches its items from the last one or picks the rows a selection changed. *)
type order = In_document_order | Out_of_order

val patch_order : order ref

val backwards :
  int ->
  Template.position ->
  (int -> Template.position -> 'made[@u]) ->
  ('made -> Dom.node Js.null -> Dom.node Js.null[@u]) ->
  'made array

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
(** Forgets the values a mounted template's holes last read, so that its next update evaluates
    and writes every hole again: for a render that threw halfway through its holes. *)
val forget : live -> unit

(** The HTML of a [dangerouslySetInnerHTML] prop; compiled code reads it through this, so that it
    needs no ppx of its own. *)
external html_of : < __html : string > Js.t -> string = "__html" [@@mel.get]

(** Puts every controlled value back that an input or change event left different from the state;
    it runs by itself after the next render, and [act] runs it at once. *)
val restore_controlled : unit -> unit

val controlled_hole : Dom.element -> string -> Hole.prop
val style_hole : Dom.element -> Hole.style
val set_style : Hole.style -> (string * string) list -> unit
val handler_hole : Dom.element -> string -> Hole.handler
val set_handler : Hole.handler -> (Dom.event -> unit) -> unit
val text_at : Template.position -> Dom.text
val put_text : input -> Dom.text -> string -> unit

val write_sole_text : Dom.element -> string -> unit
val put_sole_text : input -> Dom.element -> string -> unit
val write_attr : Dom.element -> string -> string option -> unit
val put_attr : input -> Dom.element -> string -> string option -> unit

(** The property a delegated handler for an event is kept in, the same for every element. *)
val event_key : string -> string
val put_handler : Dom.element -> string -> (Dom.event -> unit) -> unit

(** A handler site compiled once, shared by every element that uses it. *)
type ('data, 'dispatch) link

val link : ('data -> 'dispatch -> Dom.event -> unit[@u]) -> ('data, 'dispatch) link
val put_linked : Dom.element -> string -> ('data, 'dispatch) link -> 'dispatch -> 'data -> unit
val reference_hole : Dom.element -> Hole.reference
val place_reference : Hole.reference -> unit
val release_reference : Hole.reference -> unit
(** [set_reference hole identity callback]: a ref whose identity is not the one held replaces it, the
    old one called with null and the new one with the element, as React compares its refs. *)
(** Runs [attach] once the current write is over, as React attaches refs after its commit. *)
val attach_later : (unit -> unit) -> unit

(** Runs the attachments waiting for the end of the write: Melact calls it after a flush's renders. *)
val attach_references : unit -> unit

val set_reference : Hole.reference -> 'identity -> (Dom.element Js.nullable -> unit) -> unit
val content_hole : Template.position -> Hole.content
val release_content : Hole.content -> unit
val set_content : Hole.content -> child -> unit

type mounted

(** Where hydration has got to among one parent's children. *)
type cursor

val mismatch : unit -> 'a
val expected : string -> Dom.node Js.null -> 'a
val inside : Dom.node -> cursor
val adopt_element : cursor -> string -> Dom.node
val adopt_static : cursor -> string -> unit
val adopt_anchor : cursor -> unit
val adopt_text : cursor -> string -> Dom.text
(** What follows a content hole in its template: whatever comes next, or an anchor the hole ends at. *)
type ending = Before_next | Before_anchor

val adopt_content : cursor -> child -> ending -> Hole.content

(** [adopt_content] of a child at the positions given as count, index, count, index…, counted by
    [positions]. *)
val adopt_forked : cursor -> child -> ending -> int array -> Hole.content
val adopt_finish : cursor -> unit
val collected : Hole.content -> child
val collected_text : Dom.text -> string
val after_of : (live -> Dom.node Js.null -> Dom.node Js.null[@u])

val mount :
  child -> parent:'a Dom.node_like -> before:'b Dom.node_like option -> mounted

val mount_over : child -> parent:'a Dom.node_like -> mounted
val patch : mounted -> child -> unit
val unmount : mounted -> unit

(** The last value a pure expression gave and the inputs it gave it for. *)
type 'value memory

val memory : unit -> 'value memory

(** The remembered value when every input is the one it was computed from ([===]), else a new one. *)
val remember : 'value memory -> input array -> (unit -> 'value) -> 'value

(** A long list cut into groups of [size] rows, each a [tag] element with [content-visibility: auto]
    and a height estimate of [size * row_height] pixels, so that the browser skips the style, layout
    and paint of groups out of view. Rows keep their keys within a group; a row that moves to
    another group is made again there. *)
val chunks : size:int -> row_height:int -> tag:string -> child -> child
