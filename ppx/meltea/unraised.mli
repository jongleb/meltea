(** Calls of [Stdlib] functions that Melange implements by raising and catching, read by their exact
    path in the typed file ([Stdlib.float_of_string_opt], not any [float_of_string_opt]) and replaced
    by [Meltea_runtime.Numbers]' ones, which return the same without raising. *)

val structure : Melhole_typed.File.t -> Ppxlib.structure -> Ppxlib.structure
