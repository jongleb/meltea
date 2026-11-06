(** The kind of the last node written. *)
type last = Text_last | Other_last

type writer = { buffer : Buffer.t; mutable last : last }
type child = writer -> unit

let escape value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun character ->
      match character with
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#x27;"
      | other -> Buffer.add_char buffer other)
    value;
  Buffer.contents buffer

let markup writer value =
  Buffer.add_string writer.buffer value;
  writer.last <- Other_last

let markup_text writer value =
  Buffer.add_string writer.buffer value;
  writer.last <- Text_last

let static_text writer value =
  (match writer.last with Text_last -> Buffer.add_string writer.buffer "<!-- -->" | Other_last -> ());
  markup_text writer value

let dynamic_text writer value =
  match value with "" -> () | value -> static_text writer (escape value)

let empty _ = ()
let text value writer = dynamic_text writer value
let list children writer = List.iter (fun child -> child writer) children
let element write = write
let write writer child = child writer

let render child =
  let writer = { buffer = Buffer.create 1024; last = Other_last } in
  child writer;
  Buffer.contents writer.buffer

let attribute writer name value =
  Option.iter
    (fun value ->
      Buffer.add_char writer.buffer ' ';
      Buffer.add_string writer.buffer name;
      Buffer.add_string writer.buffer "=\"";
      Buffer.add_string writer.buffer (escape value);
      Buffer.add_char writer.buffer '"')
    value

let flag writer name value = attribute writer name (match value with true -> Some "" | false -> None)

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
    | kept -> Some (String.concat ";" (List.map (fun (name, value) -> hyphenated name ^ ":" ^ value) kept)))

let raw writer value = markup writer value

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
