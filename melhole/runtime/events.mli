(** Event handlers bound to elements: delegated to [document] for the common
    bubbling events, attached directly otherwise. *)

type t

val bind : Dom.element -> string -> (Dom.event -> unit) -> t
val set : t -> (Dom.event -> unit) -> unit
val release : t -> unit
