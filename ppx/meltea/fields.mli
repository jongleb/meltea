(** What [update] writes and [view] reads, by model field, as the compiler can tell from the code. *)

(** Model fields: these, or every field when the analysis cannot tell. *)
type fields = Every | Named of string list

val union : fields -> fields -> fields
val mask : fields -> int

(** What a branch of [update] leaves in one model field. *)
type change =
  | Kept  (** The value the field had. *)
  | Literals of string * string list  (** One of these constants: constructors without payload, numbers, strings. *)
  | Set  (** Any other value. *)

(** What an expression makes of the model it was given. *)
type outcome =
  | Model of (string * change) list  (** The model with these fields changed; every other field kept. *)
  | Unknown  (** A value the analysis cannot follow. *)

(** Whether every message a branch's pattern matches takes the branch. *)
type coverage =
  | Total
  | Partial  (** A guard or another part of the scrutinee decides. *)

(** A case of [update] on its message. *)
type branch = { pattern : Ppxlib.pattern; coverage : coverage; outcome : outcome }

(** The fields an outcome writes: those not [Kept], or every field. *)
val written : outcome -> fields

(** [update]'s cases on the message, read through local helpers and model-valued [let]s, also when
    it matches a tuple whose first part is the message. *)
val branches : Ppxlib.expression -> branch list option

(** What all of [update] does to each field it can write, joined over its branches; a model field
    not listed is never written, a constant after [init]. *)
val lattice : branch list -> outcome

(** The fields each message may change, as a function from the message to a bit mask. *)
val update : loc:Ppxlib.location -> Ppxlib.expression -> Ppxlib.expression option

(** The fields a view reads, as a bit mask. *)
val view : Ppxlib.expression -> int option
