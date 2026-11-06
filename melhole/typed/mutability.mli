(** What [===] on two values of a type says under Melange, where an unchanged reference is meant to be an unchanged value. *)
type t =
  | Immediate  (** Compared by value: int, char, bool, unit, constant constructors, int32, string and float are JavaScript primitives. *)
  | Immutable_boxed  (** A reference to a value that nothing changes in place: [===] is sound. *)
  | Mutable_inside  (** Holds an array, bytes, a mutable field, a ref, a [Js.dict], a setter, …: [===] can miss a change. *)
  | Unknown  (** A type variable, an abstract or open type, an OCaml object or a module: nothing is known. *)

val of_answer : Answer.t -> t
