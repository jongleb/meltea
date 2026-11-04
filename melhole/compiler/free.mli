(** What an expression reads: the variables it depends on, scope-aware. *)

type t =
  | Reads of string list  (** Pure in these free variables, sorted and distinct. *)
  | Volatile  (** It may read mutable state, or the analysis cannot tell. *)

val of_expression : Ppxlib.expression -> t
val bound : Ppxlib.pattern -> string list
