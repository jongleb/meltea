(** A traversal that knows which names are local at every point: parameters, lets, pattern
    variables. Names that are not local are module-level or library values, which never change. *)

class mapper : object
  inherit Ppxlib.Ast_traverse.map

  method locals : string list
  method definitions : (string * Ppxlib.expression) list
  (** The local [let]s in scope, by name; a definition a frontend cached, marked
      [[@melhole.cached value]], is recorded as its [value]. *)
end
