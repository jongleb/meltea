(** A traversal that knows which names are local at every point: parameters, lets, pattern
    variables. Names that are not local are module-level or library values, which never change. *)

class mapper : object
  inherit Ppxlib.Ast_traverse.map

  method locals : string list
  method definitions : (string * Ppxlib.expression) list
  (** The local [let]s in scope, by name; a definition a frontend cached, marked
      [[@melhole.cached value]], is recorded as its [value]. A definition that makes a stateful call is not one. *)

  method stateful_call : Ppxlib.expression -> [ `Stateful | `Pure ]
  (** Whether a call reads state besides its arguments, so that a [let] of it is not a definition:
      [`Pure] here; a frontend whose calls can be stateful, like Melact's hooks, overrides it. *)
end
