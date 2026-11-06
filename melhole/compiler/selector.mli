(** A projected leaf of a loop's row that differs between rows only in whether a row's key is the one
    the outer values choose: [match o with p when x = i -> a | q -> b] or [if e = i then a else b],
    where [p] binds [x] alone, [q] binds nothing, [o] and [e] read no row name, and [i], [a], [b] read
    no outer name nor [x]. [=] may be [==], and either side of it may be the key. *)

(** How the chosen key is compared with a row's. *)
type equality = Structural | Physical

type t = private {
  chosen : Ppxlib.expression;  (** The key the outer values choose, as an option. *)
  key : Ppxlib.expression;  (** A row's key, reading only the row's names. *)
  equality : equality;
}

(** [of_leaf ~outer ~row leaf]: [outer] are the local names the row reads, [row] the names its item
    and index patterns bind. *)
val of_leaf : outer:string list -> row:string list -> Ppxlib.expression -> t option
