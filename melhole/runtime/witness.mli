(** A value that proves which type it was made for, so that two of them can show their types equal,
    like [Type.Id] without the parts a bundle does not need. *)
type 'a t

val make : unit -> 'a t
val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option
