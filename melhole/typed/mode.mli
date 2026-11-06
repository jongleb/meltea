(** Whether the build asked for the typed pass, with [-typed]. *)
type t = Untyped | Typed

val current : unit -> t
