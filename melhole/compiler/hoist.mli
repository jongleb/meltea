(** Definitions lifted out of expressions to just before the structure item that needs them. *)

(** What becomes of a lifted definition that nothing refers to. *)
type unused =
  | Dropped  (** It goes. *)
  | Kept  (** It stays, for what it does. *)

val hoist : loc:Ppxlib.location -> unused:unused -> string -> Ppxlib.expression -> Ppxlib.expression
val declare : loc:Ppxlib.location -> string -> (string -> Ppxlib.type_declaration) -> string

val structure :
  (Ppxlib.structure_item -> Ppxlib.structure_item) -> Ppxlib.structure -> Ppxlib.structure
