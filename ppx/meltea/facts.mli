(** Facts about collections, read from the typed file where its code makes them: an append, a cut
    of one block, a [mapi] that writes one place or every n-th, and writes to a copied array that
    only gets stored (an exchange of two items, or a rewrite of some). Each is recorded with
    [Meltea_runtime.Delta.record], so that a compiled loop given the result patches from the fact instead
    of comparing every row. Names are exact ([Stdlib.Array.append], not any [append]), and what a
    fact names is pure: names, constants, immutable fields and arithmetic on them. *)

val structure : Melhole_typed.File.t -> Ppxlib.structure -> Ppxlib.structure
