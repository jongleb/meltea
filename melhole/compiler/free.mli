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

(** How an expression uses a variable: only through these fields ([v.f]), or as a whole value, or
    under a binding of the same name. *)
type use = Fields of Ppxlib.longident Ppxlib.loc list | Whole

val use : string -> Ppxlib.expression -> use

val leaves :
  outer:string list ->
  view:(Ppxlib.expression -> bool) ->
  Ppxlib.expression ->
  Ppxlib.expression list option