(** A rewritten file, typed by compiler-libs with the load path the compiler gives a staged rewriter. *)
type t

val of_structure : Ppxlib.structure -> t option
val typed : t -> Ocaml_common.Typedtree.structure
val env : t -> Ocaml_common.Env.t
val answer : t -> Question.t -> Answer.t option
val answers : t -> (Question.t * Answer.t) list
