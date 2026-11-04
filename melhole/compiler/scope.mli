(** A traversal that knows which names are local at every point: parameters, lets, pattern
    variables. Names that are not local are module-level or library values, which never change. *)

class mapper : object
  inherit Ppxlib.Ast_traverse.map

  method locals : string list
end
