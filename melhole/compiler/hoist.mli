(** Definitions lifted out of expressions to just before the structure item that needs them. *)

val hoist : loc:Ppxlib.location -> string -> Ppxlib.expression -> Ppxlib.expression

val structure :
  (Ppxlib.structure_item -> Ppxlib.structure_item) -> Ppxlib.structure -> Ppxlib.structure
