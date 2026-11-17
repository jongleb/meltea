(** View helpers spliced into the page. A local [let f a b = <element …>] called as a child of an
    element, with every argument a constant, a constructor or a name, is replaced there by its body
    with the arguments put in place: its static part joins the caller's template and its holes
    become the caller's, instead of a template of its own behind a content hole. A call is left as it
    is when a name could be captured either way: a name the body reads, or the helper's own, bound
    again between the helper and the call, a name of an argument bound inside the body, or a module
    opened in between. A helper whose every call is spliced is dropped. *)

val structure : Ppxlib.structure -> Ppxlib.structure
