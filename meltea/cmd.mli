(** Effects an update asks for: values describing them; the program runs them. *)

type 'msg t

val none : 'msg t
val batch : 'msg t list -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
val message : 'msg -> 'msg t
val perform : (unit -> 'value Js.Promise.t) -> ('value -> 'msg) -> 'msg t
val focus : string -> 'msg t
val port : ('value -> unit) -> 'value -> 'msg t

(** Draw the change this update makes inside [document.startViewTransition], so the browser
    animates from the old page to the new one; where it cannot, draw as usual. *)
val view_transition : 'msg t

(** How the next draw shows its changes: at once, or as a view transition of the browser. *)
type drawing = Instant | View_transition

(** What the last update asked of the next draw; the program takes it and puts back [Instant]. *)
val drawing : drawing ref
val run : 'msg t -> ('msg -> unit) -> unit
