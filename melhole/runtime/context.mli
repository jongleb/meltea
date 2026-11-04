(** A value passed down to every component below its provider. *)

type 'value t = private {
  witness : 'value Type.Id.t;
  default : 'value;  (** Seen by components with no provider above them. *)
}

val make : 'value -> 'value t
