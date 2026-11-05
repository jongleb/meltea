type site = Attr of string | Prop of string | Control of string | Handler of string | Ref | Style | Sole_text
type slot = Text_slot | Child_slot
type static = Valued of string | Bare

type 'value node =
  | Element of 'value element
  | Text of string
  | Slot of slot * 'value

and 'value element = {
  tag : string;
  statics : (string * static) list;
  dynamics : (site * 'value) list;
  children : 'value node list;
}

type 'value hole =
  | On_element of int list * site * 'value
  | At_position of int list * int * slot * 'value

(** The kind of the last static node written among siblings. *)
type previous = Nothing | Static_text | Other

let void tag =
  match tag with
  | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link"
  | "meta" | "source" | "track" | "wbr" ->
      `Void
  | _ -> `Container

let escape ~quotes text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      match (character, quotes) with
      | '&', _ -> Buffer.add_string buffer "&amp;"
      | '<', _ -> Buffer.add_string buffer "&lt;"
      | '>', _ -> Buffer.add_string buffer "&gt;"
      | '"', `Escaped -> Buffer.add_string buffer "&quot;"
      | _, (`Escaped | `Kept) -> Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let render root =
  let html = Buffer.create 256 in
  let holes = ref [] in
  let rec element path tree =
    Buffer.add_string html ("<" ^ tree.tag);
    List.iter
      (fun (name, static) ->
        match static with
        | Valued value ->
            Buffer.add_string html
              (Printf.sprintf " %s=\"%s\"" name (escape ~quotes:`Escaped value))
        | Bare -> Buffer.add_string html (" " ^ name))
      tree.statics;
    Buffer.add_char html '>';
    List.iter
      (fun (site, value) -> holes := On_element (path, site, value) :: !holes)
      tree.dynamics;
    let settle pending index =
      Option.iter
        (fun (slot, value) -> holes := At_position (path, index, slot, value) :: !holes)
        pending
    in
    let marker () = Buffer.add_string html "<!---->" in
    let rec walk nodes index previous pending =
      match (nodes, pending, previous) with
      | [], _, _ -> settle pending index
      | Text text :: rest, None, Static_text ->
          Buffer.add_string html (escape ~quotes:`Kept text);
          walk rest index Static_text None
      | Text text :: rest, Some _, Static_text ->
          marker ();
          settle pending index;
          Buffer.add_string html (escape ~quotes:`Kept text);
          walk rest (index + 2) Static_text None
      | Text text :: rest, _, (Nothing | Other) ->
          settle pending index;
          Buffer.add_string html (escape ~quotes:`Kept text);
          walk rest (index + 1) Static_text None
      | Element child :: rest, _, _ ->
          settle pending index;
          element (path @ [ index ]) child;
          walk rest (index + 1) Other None
      | Slot (slot, value) :: rest, None, _ -> walk rest index previous (Some (slot, value))
      | Slot (slot, value) :: rest, Some _, _ ->
          marker ();
          settle pending index;
          walk rest (index + 1) Other (Some (slot, value))
    in
    (match
       List.concat_map
         (fun node -> match node with Text "" -> [] | Element _ | Text _ | Slot _ -> [ node ])
         tree.children
     with
    | [ Slot (Text_slot, value) ] -> holes := On_element (path, Sole_text, value) :: !holes
    | children -> walk children 0 Nothing None);
    match void tree.tag with
    | `Void -> ()
    | `Container -> Buffer.add_string html ("</" ^ tree.tag ^ ">")
  in
  element [] root;
  (Buffer.contents html, List.rev !holes)

type step = First_of of int list | Next_of of int list
type anchor = First_or_end of int list | Next_or_end of int list

let walk holes =
  let rec needed indices =
    match List.rev indices with
    | [] -> []
    | 0 :: parent -> (indices, First_of (List.rev parent)) :: needed (List.rev parent)
    | index :: parent ->
        let previous = List.rev ((index - 1) :: parent) in
        (indices, Next_of previous) :: needed previous
  in
  List.sort_uniq compare
    (List.concat_map
       (fun hole ->
         match hole with
         | On_element (indices, _, _) | At_position (indices, 0, _, _) -> needed indices
         | At_position (indices, index, _, _) -> needed indices @ needed (indices @ [ index - 1 ]))
       holes)

let anchor indices index =
  match index with 0 -> First_or_end indices | _ -> Next_or_end (indices @ [ index - 1 ])
