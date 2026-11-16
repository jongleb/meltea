(** What compiled code proved about how one collection became another: facts about two values,
    told apart by identity. A fact lives as long as the value it describes and does not keep the
    value it came from alive. Linking this module is what lets compiled row loops patch from facts;
    each kind is its own value, so a bundle carries only the kinds its code records. *)

(** One kind of fact: what it tells a loop, from its old and new items. *)
type kind

(** The items before, then new ones. *)
val appended : kind

(** [cut from until]: the items before, without those from [from] to before [until]. *)
val cut : int -> int -> kind

(** [exchanged first second]: the items before with these two places exchanged. *)
val exchanged : int -> int -> kind

(** [rewritten places]: the items before, except that these places may hold new items. *)
val rewritten : int array -> kind

(** [strided step from], with [step > 0] and [from >= 0]: the items before, except at [from], [from + step], .... *)
val strided : int -> int -> kind

(** [record kind before after] records that [after] is [before] changed as [kind] says, and gives
    [after]. *)
val record : kind -> 'collection -> 'collection -> 'collection
