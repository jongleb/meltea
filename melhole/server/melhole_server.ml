module Tree = struct
  type t = { id : int; overflow : string }

  let root = { id = 1; overflow = "" }

  let bits number =
    let rec count number found = match number with 0 -> found | _ -> count (number lsr 1) (found + 1) in
    count number 0

  let base32 number =
    let rec spelled number found =
      let digit = String.make 1 "0123456789abcdefghijklmnopqrstuv".[number land 31] in
      match number < 32 with true -> digit ^ found | false -> spelled (number lsr 5) (digit ^ found)
    in
    spelled number ""

  let pushed tree count index =
    let base_length = bits tree.id - 1 in
    let base = tree.id land lnot (1 lsl base_length) in
    let slot = index + 1 in
    match bits count + base_length > 30 with
    | false -> { tree with id = (1 lsl (bits count + base_length)) lor (slot lsl base_length) lor base }
    | true ->
        let overflowing = base_length - (base_length mod 5) in
        let rest_length = base_length - overflowing in
        {
          id = (1 lsl (bits count + rest_length)) lor (slot lsl rest_length) lor (base lsr overflowing);
          overflow = base32 (base land ((1 lsl overflowing) - 1)) ^ tree.overflow;
        }

  let id tree local =
    ":R"
    ^ base32 (tree.id land lnot (1 lsl (bits tree.id - 1)))
    ^ tree.overflow
    ^ (match local with 0 -> "" | _ -> "H" ^ base32 local)
    ^ ":"
end

(** The kind of the last node written. *)
type last = Text_last | Other_last

(** Where the HTML goes: a buffer natively, a string under Melange, where JavaScript joins strings cheaply. *)
type sink = Buffered of Buffer.t | Joined of { mutable text : string }

(** What the [select] being written chose. *)
type choice = Unchosen | Chosen of string

type marked = Marked | Unmarked

(** Whether the HTML keeps what hydration needs: the comments between texts and around boundaries. *)
type format = Hydratable_html | Static_html

type writer = {
  sink : sink;
  mutable last : last;
  mutable tree : Tree.t;
  mutable choice : choice;
  format : format;
}

type child =
  | Empty
  | Text of string
  | Listed of child list
  | Numbered of child array  (** A JavaScript array: React numbers each item's position. *)
  | Written of (writer -> unit)

let added writer value =
  match writer.sink with Buffered buffer -> Buffer.add_string buffer value | Joined joined -> joined.text <- joined.text ^ value

let escape value =
  let length = String.length value in
  let rec special index =
    match index = length with
    | true -> length
    | false -> (
        match String.unsafe_get value index with '&' | '<' | '>' | '"' | '\'' -> index | _ -> special (index + 1))
  in
  match special 0 with
  | first when first = length -> value
  | first ->
      let buffer = Buffer.create (length + 16) in
      Buffer.add_substring buffer value 0 first;
      for index = first to length - 1 do
        match String.unsafe_get value index with
        | '&' -> Buffer.add_string buffer "&amp;"
        | '<' -> Buffer.add_string buffer "&lt;"
        | '>' -> Buffer.add_string buffer "&gt;"
        | '"' -> Buffer.add_string buffer "&quot;"
        | '\'' -> Buffer.add_string buffer "&#x27;"
        | other -> Buffer.add_char buffer other
      done;
      Buffer.contents buffer

let compiled writer value =
  match writer.format with
  | Hydratable_html -> added writer value
  | Static_html ->
      let length = String.length value in
      let buffer = Buffer.create length in
      let rec from index =
        match (index < length, index + 8 <= length && String.equal (String.sub value index 8) "<!-- -->") with
        | false, _ -> ()
        | true, true -> from (index + 8)
        | true, false ->
            Buffer.add_char buffer (String.unsafe_get value index);
            from (index + 1)
      in
      from 0;
      added writer (Buffer.contents buffer)

let markup writer value =
  compiled writer value;
  writer.last <- Other_last

let markup_text writer value =
  compiled writer value;
  writer.last <- Text_last

let static_text writer value =
  (match (writer.last, writer.format) with
  | Text_last, Hydratable_html -> added writer "<!-- -->"
  | Text_last, Static_html | Other_last, (Hydratable_html | Static_html) -> ());
  markup_text writer value

let hydration_markup writer value =
  match writer.format with
  | Hydratable_html -> markup writer value
  | Static_html -> writer.last <- Other_last

let dynamic_text writer value =
  match value with "" -> () | value -> static_text writer (escape value)

let leading_newline writer value =
  match String.length value > 0 && Char.equal (String.unsafe_get value 0) '\n' with
  | true -> added writer "\n"
  | false -> ()

let empty = Empty
let text value = Text value
let list children = Listed children
let array children = Numbered children
let element write = Written write

let position writer = writer.tree

let rec write writer child =
  match child with
  | Empty -> ()
  | Text value -> dynamic_text writer value
  | Listed children -> List.iter (write writer) children
  | Numbered children ->
      let tree = writer.tree and count = Array.length children in
      Array.iteri
        (fun index child ->
          writer.tree <- Tree.pushed tree count index;
          write writer child)
        children;
      writer.tree <- tree
  | Written write -> write writer

let forked writer path child =
  let tree = writer.tree in
  for at = 0 to (Array.length path / 2) - 1 do
    writer.tree <- Tree.pushed writer.tree (Array.unsafe_get path (2 * at)) (Array.unsafe_get path ((2 * at) + 1))
  done;
  write writer child;
  writer.tree <- tree

let items child =
  match child with
  | Empty -> []
  | Listed children -> children
  | Numbered children -> Array.to_list children
  | Text _ | Written _ -> [ child ]

let rec texts child =
  match child with
  | Text value -> value
  | Listed children -> String.concat "" (List.map texts children)
  | Numbered children -> String.concat "" (Array.to_list (Array.map texts children))
  | Empty | Written _ -> ""

let recover child replacement =
  Written
    (fun writer ->
      let last = writer.last and tree = writer.tree and choice = writer.choice in
      let restore =
        match writer.sink with
        | Buffered buffer ->
            let length = Buffer.length buffer in
            fun () -> Buffer.truncate buffer length
        | Joined joined ->
            let text = joined.text in
            fun () -> joined.text <- text
      in
      match write writer child with
      | () -> ()
      | exception error ->
          restore ();
          writer.last <- last;
          writer.tree <- tree;
          writer.choice <- choice;
          write writer (replacement error))

let rendered format child =
  let writer =
    {
      sink = (match Sys.backend_type with Native | Bytecode -> Buffered (Buffer.create 1024) | Other _ -> Joined { text = "" });
      last = Other_last;
      tree = Tree.root;
      choice = Unchosen;
      format;
    }
  in
  write writer child;
  match writer.sink with Buffered buffer -> Buffer.contents buffer | Joined joined -> joined.text

let render child = rendered Hydratable_html child
let render_static child = rendered Static_html child

let attribute writer name value =
  Option.iter
    (fun value ->
      added writer " ";
      added writer name;
      added writer "=\"";
      added writer (escape value);
      added writer "\"")
    value

let flag writer name value = attribute writer name (match value with true -> Some "" | false -> None)

let choose writer value =
  let around = writer.choice in
  writer.choice <- Option.fold ~none:Unchosen ~some:(fun value -> Chosen value) value;
  around

let chosen writer choice = writer.choice <- choice

let option_selected writer value marked =
  match (writer.choice, marked) with
  | Chosen chosen, _ when String.equal chosen value -> added writer " selected=\"\""
  | Chosen _, (Marked | Unmarked) | Unchosen, Unmarked -> ()
  | Unchosen, Marked -> added writer " selected=\"\""

let style writer declarations =
  let hyphenated name =
    match String.length name >= 2 && String.equal (String.sub name 0 2) "--" with
    | true -> name
    | false ->
    let buffer = Buffer.create (String.length name + 4) in
    String.iter
      (fun character ->
        match character with
        | 'A' .. 'Z' ->
            Buffer.add_char buffer '-';
            Buffer.add_char buffer (Char.lowercase_ascii character)
        | other -> Buffer.add_char buffer other)
      name;
    let made = Buffer.contents buffer in
    match String.length made >= 3 && String.equal (String.sub made 0 3) "ms-" with
    | true -> "-" ^ made
    | false -> made
  in
  attribute writer "style"
    (match List.filter (fun (_, value) -> not (String.equal value "")) declarations with
    | [] -> None
    | kept -> Some (String.concat ";" (List.map (fun (name, value) -> hyphenated name ^ ":" ^ String.trim value) kept)))

let raw writer value =
  added writer value;
  writer.last <- Other_last

let float value =
  match (Float.is_nan value, Float.is_finite value, Float.equal value 0.) with
  | true, _, _ -> "NaN"
  | false, false, _ -> if value > 0. then "Infinity" else "-Infinity"
  | false, true, true -> "0"
  | false, true, false ->
      let negative = value < 0. in
      let magnitude = Float.abs value in
      let long = Printf.sprintf "%.40e" magnitude in
      let at = String.index long 'e' in
      let expanded = String.concat "" (String.split_on_char '.' (String.sub long 0 at)) in
      let exponent = int_of_string (String.sub long (at + 1) (String.length long - at - 1)) in
      let back digits exponent =
        match Float.equal (float_of_string (digits ^ "e" ^ string_of_int (exponent - String.length digits + 1))) magnitude with
        | true -> Some (digits, exponent)
        | false -> None
      in
      let rec shortest precision =
        let lower = String.sub expanded 0 precision in
        let upper, raised =
          let bytes = Bytes.of_string lower in
          let rec carry index =
            match index < 0 with
            | true -> `Overflow
            | false -> (
                match Bytes.get bytes index with
                | '9' ->
                    Bytes.set bytes index '0';
                    carry (index - 1)
                | digit ->
                    Bytes.set bytes index (Char.chr (Char.code digit + 1));
                    `Fits)
          in
          match carry (precision - 1) with
          | `Fits -> (Bytes.to_string bytes, exponent)
          | `Overflow -> ("1" ^ Bytes.to_string bytes, exponent + 1)
        in
        let rest = String.sub expanded precision (String.length expanded - precision) in
        let half = "5" ^ String.make (String.length rest - 1) '0' in
        match (back lower exponent, back upper raised, compare rest half) with
        | None, None, _ when precision < 17 -> shortest (precision + 1)
        | None, None, _ -> (lower, exponent)
        | Some found, None, _ | None, Some found, _ -> found
        | Some below, Some _, order when order < 0 -> below
        | Some _, Some above, order when order > 0 -> above
        | Some below, Some above, _ -> (
            match (Char.code lower.[precision - 1] - Char.code '0') mod 2 with 0 -> below | _ -> above)
      in
      let found, exponent = shortest 1 in
      let digits =
        let rec trimmed text =
          match String.length text > 1 && text.[String.length text - 1] = '0' with
          | true -> trimmed (String.sub text 0 (String.length text - 1))
          | false -> text
        in
        trimmed found
      in
      let count = String.length digits in
      let point = exponent + 1 in
      let written =
        match (point > 21, point <= -6) with
        | false, false when point >= count -> digits ^ String.make (point - count) '0'
        | false, false when point > 0 -> String.sub digits 0 point ^ "." ^ String.sub digits point (count - point)
        | false, false -> "0." ^ String.make (-point) '0' ^ digits
        | _, _ ->
            let fraction = match count with 1 -> "" | _ -> "." ^ String.sub digits 1 (count - 1) in
            let shown = point - 1 in
            String.sub digits 0 1 ^ fraction ^ "e" ^ (if shown >= 0 then "+" else "-") ^ string_of_int (abs shown)
      in
      if negative then "-" ^ written else written

let capture =
  {|(function(g,d){var q=[],t=["click","dblclick","input","change","keydown","keyup","submit"],f=function(e){var x=e.target;q.push([e,x.value,x.checked])};t.forEach(function(n){d.addEventListener(n,f,true)});g.__melhole_early=function(){t.forEach(function(n){d.removeEventListener(n,f,true)});delete g.__melhole_early;return q}})(globalThis,document)|}

let data ~id json =
  let buffer = Buffer.create (String.length json + 64) in
  Buffer.add_string buffer "<script type=\"application/json\" id=\"";
  Buffer.add_string buffer (escape id);
  Buffer.add_string buffer "\">";
  String.iter
    (fun character ->
      match character with '<' -> Buffer.add_string buffer "\\u003c" | other -> Buffer.add_char buffer other)
    json;
  Buffer.add_string buffer "</script>";
  Buffer.contents buffer
