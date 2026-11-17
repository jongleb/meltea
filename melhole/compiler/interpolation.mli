(** Melange's interpolated strings, [{j|… $name … $(name) …|j}], written as the concatenation
    melange.ppx makes of them, so that the names they read are seen by every pass; one that is not
    valid stays as it is, for melange.ppx to report. *)

val expanded : Ppxlib.structure -> Ppxlib.structure
