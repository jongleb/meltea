(** Compiled loops over a source that is the same value at every draw: a value bound at the top of a
    module, read by its exact path in the typed file. Such a loop never has other items to compare,
    so [Meltea_runtime.View.rows] becomes [Meltea_runtime.View.fixed], with no keys, scans or moves. *)

val structure : Melhole_typed.File.t -> Ppxlib.structure -> Ppxlib.structure
