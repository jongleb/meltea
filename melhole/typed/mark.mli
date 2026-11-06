(** An attribute [[@name n]] that the untyped rewrite leaves on a node for the typed pass, which removes it. *)

val make : Ppxlib.location -> string -> int -> Ppxlib.attribute
val number : Ppxlib.payload -> int option
val find : string -> Ppxlib.attributes -> int option
val without : string -> Ppxlib.attributes -> Ppxlib.attributes
