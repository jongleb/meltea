(** The shared template compiler: lowercase JSX trees become one template and its holes. *)

(** Which text function of the front end a child calls. *)
type text = String_text | Int_text | Float_text

(** A child the front end recognises. *)
type special =
  | Text of text * Ppxlib.expression  (** [React.string e], [Html.int e]. *)
  | Nothing  (** [React.null], [Html.none]. *)

(** An event prop, for the front end to turn into a handler hole. *)
type handler = {
  label : string;  (** The JSX label, like [onClick]. *)
  event : string;  (** The DOM event, like [click]. *)
  requirement : [ `Required | `Optional ];
  value : Ppxlib.expression;
}

(** How a front end's handlers reach their elements. *)
type handlers =
  | Closures  (** A closure per element, written whenever the values it reads change. *)
  | Linked of string
      (** One handler per site; the element carries the locals it reads, apart from this dispatch. *)

(** What a front end does after a render throws halfway through a template. *)
type throwing =
  | Recovers  (** Renders again, so a template must be able to forget what it wrote. *)
  | Stops

(** Whether a front end numbers the positions of children as React's [useId] does. *)
type ids = Tree_ids | No_ids

(** Whether the compiler already guards a hole by the locals it reads. *)
type guarded = Guarded | Unguarded

(** When a hole of a template whose front end knows what changed is evaluated. *)
type gate =
  | Gated of { mask : int; unmasked : string list }
      (** When [opened] meets [mask], or one of these locals is not the one it was. *)
  | Always

(** How a template's refresh is narrowed to what changed since its last one. *)
type gates =
  | Ungated
  | Gates of {
      opened : Ppxlib.expression -> Ppxlib.expression;
          (** Given the frame's first slot, the bits that changed, or every bit for a new frame. *)
      gate : Ppxlib.expression -> guarded -> gate;
    }

(** How a front end shapes elements around the templates. *)
type frontend = {
  name : string;  (** For error messages. *)
  locals : string list;  (** Names local at the JSX site; only they can change between renders. *)
  definitions : (string * Ppxlib.expression) list;  (** Local lets in scope with their values. *)
  map : Ppxlib.expression -> Ppxlib.expression;  (** The front end's own rewriting of sub-expressions. *)
  wrap : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression;  (** A [View.child] into an element. *)
  unwrap : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression;  (** An element into a [View.child]. *)
  handler : Ppxlib.location -> handler -> Ppxlib.expression;  (** The hole value of an event prop. *)
  handlers : handlers;
  throwing : throwing;
  style : Ppxlib.location -> [ `Required | `Optional ] -> Ppxlib.expression -> Ppxlib.expression;
      (** The hole value of a [style] prop. *)
  reference : Ppxlib.location -> [ `Required | `Optional ] -> Ppxlib.expression -> Ppxlib.expression;
      (** The hole value of a [ref] prop: the callback, or a pair of what tells the ref from another and the callback. *)
  inner_html : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression;
      (** The HTML of a [dangerouslySetInnerHTML] value. *)
  ids : ids;
  classify : Ppxlib.expression -> special option;
  extra : string -> Dom_props.kind option;  (** Props the front end adds to reason-react's. *)
  each : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression option;
      (** A keyed loop for a child that maps a row function over a list. *)
  gates : Ppxlib.location -> gates;  (** How the template at this site is narrowed. *)
}

(** Whether compiled templates can take over a server's HTML; the [-hydratable] flag of the ppx. *)
type output = Client_only | Hydratable | Server

val output : output ref
val without_jsx : Ppxlib.attributes -> Ppxlib.attributes option

(** The positions a child slot is at, as count, index, count, index…, when its front end numbers them. *)
val fork_path : Ppxlib.expression -> Ppxlib.expression option
val list_items : Ppxlib.expression -> Ppxlib.expression list option

(** Whether a loop's row function takes the item, or the index and then the item. *)
type numbering = Items | Numbered

val loop :
  frontend ->
  loc:Ppxlib.location ->
  definitions:(string * Ppxlib.expression) list ->
  view:(Ppxlib.expression -> bool) ->
  stable:Ppxlib.expression list ->
  render:(Ppxlib.expression -> Ppxlib.expression) ->
  numbering:numbering ->
  Ppxlib.expression ->
  Ppxlib.expression ->
  Ppxlib.expression option

val lowercase :
  frontend ->
  loc:Ppxlib.location ->
  attributes:Ppxlib.attributes ->
  string ->
  (Ppxlib.arg_label * Ppxlib.expression) list ->
  Ppxlib.expression

val fragment : frontend -> loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
