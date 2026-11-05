(** Fanout: which frequent events reach which lists of the view. An [onInput], a key or a pointer
    move sends a message, its branch of [update] writes model fields, and a list in [view] that
    reads one of them checks all its rows on every such event. The compiler sees the whole path,
    with no annotations, and says so at the handler with an alert, [fanout]. *)

(** The module's structure, with an alert at every handler whose events reach a list. *)
val annotate : Ppxlib.structure -> Ppxlib.structure
