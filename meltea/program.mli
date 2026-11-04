(** A running Elm-architecture program. *)

type 'msg t

val sandbox :
  init:'model ->
  update:('msg -> 'model -> 'model) ->
  view:('model -> 'msg Html.t) ->
  Dom.element ->
  'msg t

val element :
  init:'model * 'msg Cmd.t ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  view:('model -> 'msg Html.t) ->
  subscriptions:('model -> 'msg Sub.t) ->
  Dom.element ->
  'msg t

val dispatch : 'msg t -> 'msg -> unit
val flush : 'msg t -> unit
val stop : 'msg t -> unit
