(** The stylesheets a module declares with [[@@@meltea.styles "file.css"]], read at build time, and
    what they would see if the rows of a list were cut into groups: a selector that counts siblings
    ([:first-child], [:nth-child] and the like), a child combinator from the list's element to a
    row, or a sibling combinator between rows. *)

(** What the compiler knows of an element: its tag, if known, and its classes, if they are a
    constant. *)
type element = { tag : string option; classes : string list option }

(** The selectors of a module's declared stylesheets, or none declared. *)
type t = Declared of (string * string) list | Undeclared

val read : Ppxlib.structure -> t

(** The first selector a list's rows grouped inside its element would change, with its file. *)
val breaking : t -> container:element -> row:element -> (string * string) option
