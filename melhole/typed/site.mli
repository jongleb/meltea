(** Code the untyped rewrite leaves for the typed pass to decide, innermost first, after every rewriter that uses it. *)

val expression : Ppxlib.expression -> decide:(File.t -> Ppxlib.expression -> Ppxlib.expression) -> Ppxlib.expression
val choice :
  Ppxlib.expression ->
  among:('key * Ppxlib.expression) list ->
  decide:(File.t -> 'key option) ->
  Ppxlib.expression

val items : Ppxlib.structure -> decide:(File.t -> Ppxlib.structure -> Ppxlib.structure) -> Ppxlib.structure
