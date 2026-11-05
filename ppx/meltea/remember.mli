(** Derived values in a view, remembered by what they read: [let shown = List.filter … model.items]
    is computed again only when [model.items] or another input changed ([===]), and otherwise is
    the same object, so the holes that read it are not written. *)

(** A [view] binding with every derived [let] at the top of its body remembered. *)
val structure_item : Ppxlib.structure_item -> Ppxlib.structure_item
