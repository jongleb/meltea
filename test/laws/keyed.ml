type t = (string, int) Hashtbl.t

let make () = Hashtbl.create 64
let find table key = Option.value ~default:(-1) (Hashtbl.find_opt table key)
let set table key value = Hashtbl.replace table key value
