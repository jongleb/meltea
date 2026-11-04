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
  family : string;  (** The [ReactEvent] family of the prop. *)
  requirement : [ `Required | `Optional ];
  value : Ppxlib.expression;
}

(** How a front end shapes elements around the templates. *)
type frontend = {
  name : string;  (** For error messages. *)
  locals : string list;  (** Names local at the JSX site; only they can change between renders. *)
  map : Ppxlib.expression -> Ppxlib.expression;  (** The front end's own rewriting of sub-expressions. *)
  wrap : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression;  (** A [View.child] into an element. *)
  unwrap : Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression;  (** An element into a [View.child]. *)
  handler : Ppxlib.location -> handler -> Ppxlib.expression;  (** The hole value of an event prop. *)
  style : Ppxlib.location -> [ `Required | `Optional ] -> Ppxlib.expression -> Ppxlib.expression;
      (** The hole value of a [style] prop. *)
  reference : Ppxlib.location -> [ `Required | `Optional ] -> Ppxlib.expression -> Ppxlib.expression;
      (** The hole value of a [ref] prop. *)
  classify : Ppxlib.expression -> special option;
  extra : string -> Dom_props.kind option;  (** Props the front end adds to reason-react's. *)
}

val without_jsx : Ppxlib.attributes -> Ppxlib.attributes option
val list_items : Ppxlib.expression -> Ppxlib.expression list option

val lowercase :
  frontend -> loc:Ppxlib.location -> string -> (Ppxlib.arg_label * Ppxlib.expression) list -> Ppxlib.expression

val fragment : frontend -> loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
