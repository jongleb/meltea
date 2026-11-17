(** What Melange's [Stdlib] computes by raising and catching, computed without raising: a raised
    exception builds a JavaScript error with its stack, and [float_of_string] builds one even when it
    succeeds. The typed pass of [meltea-ppx] calls these in place of [Stdlib]'s. *)

val float_of_string_opt : string -> float option
