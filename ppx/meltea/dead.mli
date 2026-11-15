(** Dead messages and dead handlers. A message that nothing in the module builds outside its own
    branch of [update] (no handler, command, subscription or [init]) never arrives unless another
    module sends it; a handler whose every message leaves the model as it is and runs no command does
    nothing. The compiler sees both with no annotations and says so: an alert, [dead_message], at
    the message's constructor, and [dead_handler] at the handler. *)

(** The module's structure, with an alert at every dead message and every dead handler. *)
val annotate : Ppxlib.structure -> Ppxlib.structure
