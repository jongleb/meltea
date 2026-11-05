let json text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | character when Char.code character < 0x20 -> Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code character))
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let () =
  let state = Random.State.make [| 20261105 |] in
  let floats =
    List.init 400 (fun _ ->
        match Random.State.int state 3 with
        | 0 -> Random.State.float state 1e6 -. 5e5
        | 1 -> Int64.float_of_bits (Random.State.int64 state Int64.max_int)
        | _ -> Float.of_int (Random.State.int state 100000) /. 1000.)
  in
  let pages =
    List.init 300 (fun _ ->
        let tape = Array.init (Random.State.int state 60) (fun _ -> Random.State.int state 1000) in
        Printf.sprintf "{\"tape\":[%s],\"html\":%s}"
          (String.concat "," (Array.to_list (Array.map string_of_int tape)))
          (json (Program.render ~view:Pages.view (Pages.decode tape))))
  in
  print_string
    (Printf.sprintf "{\"pages\":[%s],\"floats\":[%s]}" (String.concat "," pages)
       (String.concat ","
          (List.map
             (fun value -> Printf.sprintf "{\"bits\":\"%Ld\",\"text\":%s}" (Int64.bits_of_float value) (json (Melhole_server.float value)))
             floats)))
