(** A running Elm-architecture program. *)

type 'msg t

(** What the compiler found in [update] and [view], as bit masks over model fields: the fields
    each message may change and the fields the view reads. *)
type 'msg tracking = { changes : 'msg -> int; reads : int }

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
