(** The identity of one [@react.component], made once per definition and compared physically. *)

type t = private { name : string  (** For error messages and devtools. *) }

val make : name:string -> t
