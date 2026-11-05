(** The constructors that tell witnesses apart, one per [make]. *)
type _ key = ..

(** One witness: a constructor of [key] for its type. *)
module type Key = sig
  type a
  type _ key += Key : a key
end

type 'a t = (module Key with type a = 'a)

let make (type value) () : value t =
  (module struct
    type a = value
    type _ key += Key : a key
  end)

let equal (type a b) ((module A) : a t) ((module B) : b t) : (a, b) Type.eq option =
  match A.Key with B.Key -> Some Type.Equal | _ -> None
