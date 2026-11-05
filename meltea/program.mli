(** A running Elm-architecture program. *)

type 'msg t

(** How a program takes over its root: empty, or holding the server's HTML of the first model. *)
type start

val mount : start
val hydrate : start

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

(** What the compiler found in [update] and [view], checked to describe the functions in use. *)
type 'msg tracking

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
  subscriptions:'subscriptions ->
  listens:reads ->
  'msg tracking

val sandbox :
  ?tracking:'msg tracking ->
  ?start:start ->
  init:'model ->
  update:('msg -> 'model -> 'model) ->
  view:('model -> 'msg Html.t) ->
  Dom.element ->
  'msg t

val element :
  ?tracking:'msg tracking ->
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
