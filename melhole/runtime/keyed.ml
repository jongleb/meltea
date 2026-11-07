type t

external make : unit -> t = "Map" [@@mel.new]
external get : t -> string -> int Js.undefined = "get" [@@mel.send]
external set : t -> string -> int -> unit = "set" [@@mel.send]

let find table key = Option.value ~default:(-1) (Js.Undefined.toOption (get table key))
