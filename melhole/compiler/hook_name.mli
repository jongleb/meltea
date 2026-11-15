(** What a function's name says about it by the Rules of React: [use], or [use] followed by anything but a
    lowercase letter, names a hook. *)
type t =
  | Hook of { base : string; dependencies : string }
      (** A hook; [base] is its name without trailing digits, [dependencies] those digits. *)
  | Plain  (** Any other name, [user] and [useless] included. *)

val of_name : string -> t
