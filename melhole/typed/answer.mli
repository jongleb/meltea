(** The type of a marked expression or external of a typed file, with the environment that defines its names. *)
type t = private { env : Ocaml_common.Env.t; type_expr : Ocaml_common.Types.type_expr }

(** How an argument is passed. *)
type label = Unlabelled | Labelled of string | Optional of string

(** What a named type is made of, where it is defined. *)
type representation =
  | Abstract  (** No definition is visible. *)
  | Unboxed of t  (** One constructor or field, represented as the value it holds. *)
  | Defined  (** A record, a variant or an open type. *)

(** Whether a JavaScript object type lists all of its fields. *)
type row = Closed | Open

(** The outermost shape of a type, after expanding abbreviations. *)
type view =
  | Function of label * t * t
  | Named of { path : string; arguments : t list; representation : representation }
      (** [path] from the top, module aliases resolved: [React.element], [Js.dict], [Stdlib__Hashtbl.t]. *)
  | Js_object of { fields : (string * t) list; row : row }  (** [< … > Js.t]; a setter is a field named [name#=]. *)
  | Other  (** A type variable, a tuple, a polymorphic variant, an OCaml object or a module. *)

val make : Ocaml_common.Env.t -> Ocaml_common.Types.type_expr -> t
val view : t -> view
val name : Ocaml_common.Env.t -> Ocaml_common.Path.t -> string
