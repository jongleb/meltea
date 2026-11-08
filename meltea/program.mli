(** A running Elm-architecture program. *)

type 'msg t

(** How a program takes over its root: empty, or holding the server's HTML of the first model. *)
type start

val mount : start
val hydrate : start

(** Like [hydrate], but the server's page stays as it is, without running the view, until the first
    input inside it (a click, a key, typing, a pointer press) or the first draw of a message from a
    command or subscription; then it is taken over at once and the input or the draw goes on. Events
    kept before the code arrived are replayed at once, as with [hydrate]. *)
val resume : start

(** For development: take the model of the program running on the root, when the module's types
    are the same, so that new code shows the old state; otherwise start from [init]. *)
val hot : start

(** The model fields each message may change, as a bit mask, with the [update] they were found in. *)
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

(** The model fields a view reads, as a bit mask, with the view they were found in. *)
type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread

(** The type declarations of the module that defines [update], as a digest. *)
type shape = Shape of string | Unshaped

(** The view's page as a function of the bits of the fields changed since its last draw, with the view it was found in. *)
type ('model, 'msg) slice =
  | Sliced : { analysed : 'view; draw : int -> 'model -> 'msg Html.t } -> ('model, 'msg) slice
  | Unsliced

(** What the compiler found in [update] and [view], checked to describe the functions in use. *)
type ('model, 'msg) tracking

(** The bits a template's refresh is narrowed to: [changed], or every bit while its frame is new. *)
val opened : Melhole.View.input -> int -> int

val mutable_read : unit
[@@alert
  mutable_model
    "this model field can change in place, and Meltea redraws only when the model value changes: keep the model immutable"]

val track :
  update:'update ->
  changes:'msg changes ->
  view:'view ->
  reads:reads ->
  shape:shape ->
  slice:('model, 'msg) slice ->
  subscriptions:'subscriptions ->
  listens:reads ->
  ('model, 'msg) tracking

val sandbox :
  ?tracking:('model, 'msg) tracking ->
  ?start:start ->
  init:'model ->
  update:('msg -> 'model -> 'model) ->
  view:('model -> 'msg Html.t) ->
  Dom.element ->
  'msg t

val element :
  ?tracking:('model, 'msg) tracking ->
  ?start:start ->
  init:'model * 'msg Cmd.t ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  view:('model -> 'msg Html.t) ->
  subscriptions:('model -> 'msg Sub.t) ->
  Dom.element ->
  'msg t

val render : view:('model -> 'msg Html.t) -> 'model -> string
val dispatch : 'msg t -> 'msg -> unit
val flush : 'msg t -> unit
val stop : 'msg t -> unit
