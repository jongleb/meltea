(** What the untyped rewrite asks of types: the type of one expression or external, marked [[@melhole.ask n]]. *)
type t

val expression : Ppxlib.expression -> t * Ppxlib.expression
val value : Ppxlib.value_description -> t * Ppxlib.value_description
val of_attributes : Ocaml_common.Typedtree.attributes -> t option
val unmarked : Ppxlib.attributes -> Ppxlib.attributes
