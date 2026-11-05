(** Enter, leave and move animations driven by CSS classes, as Vue's [<Transition>] and
    [<TransitionGroup>]. The wrapper stays mounted while what it shows changes. An element that
    appears gets [name-enter-from] and [name-enter-active], then [name-enter-to] from the next
    frame until its transition ends. An element that goes stays where it was with the [leave]
    classes until its transition ends, then leaves the DOM. [group] also slides the elements that
    stay but move, with [name-move], as FLIP does. The first render does not animate. The time is
    [duration] milliseconds, or what the element's computed style says. *)

val make : name:string -> ?duration:int -> View.child -> View.child
val group : name:string -> ?duration:int -> View.child -> View.child
