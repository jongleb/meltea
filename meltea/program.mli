(** A running Elm-architecture program. *)

type 'msg t

(** The model fields each message may change, as a bit mask, with the [update] they were found in. *)
type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

(** The model fields a view reads, as a bit mask, with the view they were found in. *)
type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread

(** What the compiler found in [update] and [view], checked to describe the functions in use. *)
type 'msg tracking

val track : update:'update -> changes:'msg changes -> view:'view -> reads:reads -> 'msg tracking

val sandbox :
  ?tracking:'msg tracking ->
  init:'model ->
  update:('msg -> 'model -> 'model) ->
  view:('model -> 'msg Html.t) ->
  Dom.element ->
  'msg t

val element :
  ?tracking:'msg tracking ->
  init:'model * 'msg Cmd.t ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  view:('model -> 'msg Html.t) ->
  subscriptions:('model -> 'msg Sub.t) ->
  Dom.element ->
  'msg t

val dispatch : 'msg t -> 'msg -> unit
val flush : 'msg t -> unit
val stop : 'msg t -> unit
