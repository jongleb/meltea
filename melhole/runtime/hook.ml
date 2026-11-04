type 'a key = Key of 'a Type.Id.t [@@unboxed]

(** A hook's value with the key of the call site that made it. *)
type slot = Slot : 'a key * 'a -> slot

type slots = { mutable items : slot array; mutable count : int; mutable cursor : int }

let key () = Key (Type.Id.make ())
let slots () = { items = [||]; count = 0; cursor = 0 }
let rewind slots = slots.cursor <- 0

let take : type a. slots -> a key -> (unit -> a) -> a =
 fun slots (Key witness as key) init ->
  let at = slots.cursor in
  slots.cursor <- at + 1;
  if at < slots.count then
    let (Slot (Key stored, value)) = slots.items.(at) in
    match Type.Id.provably_equal stored witness with
    | Some Equal -> value
    | None ->
        Js.Exn.raiseError
          "melhole: hooks were called in a different order than in the previous render"
  else
    let value = init () in
    let slot = Slot (key, value) in
    if at = Array.length slots.items then begin
      let grown = Array.make (max 4 (2 * at)) slot in
      Array.blit slots.items 0 grown 0 at;
      slots.items <- grown
    end;
    slots.items.(at) <- slot;
    slots.count <- at + 1;
    value
