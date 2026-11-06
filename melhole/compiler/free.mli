(** What an expression reads: the variables it depends on, scope-aware. *)

type t =
  | Reads of string list  (** Pure in these free variables, sorted and distinct. *)
  | Volatile  (** It may read mutable state, or the analysis cannot tell. *)

val of_expression : Ppxlib.expression -> t

(** What a value captures: like [of_expression], but the body of a function in it is read as when
    the function is called, so that what the body reads from mutable state is not a capture. *)
val captured : Ppxlib.expression -> t

val closure :
  definitions:(string * Ppxlib.expression) list -> locals:string list -> Ppxlib.expression -> t
val bound : Ppxlib.pattern -> string list

val leaves :
  outer:string list ->
  view:(Ppxlib.expression -> bool) ->
  Ppxlib.expression ->
  Ppxlib.expression list option