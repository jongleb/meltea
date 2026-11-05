(** What [update] writes and [view] reads, by model field, as the compiler can tell from the code. *)

(** Model fields: these, or every field when the analysis cannot tell. *)
type fields = Every | Named of string list

val union : fields -> fields -> fields
val mask : fields -> int

(** [update]'s cases on the message, each with the fields its branch writes. *)
val branches : Ppxlib.expression -> (Ppxlib.case * fields) list option

(** The fields each message may change, as a function from the message to a bit mask. *)
val update : loc:Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression option

(** The fields a view reads, as a bit mask. *)
val view : Ppxlib.expression -> int option
