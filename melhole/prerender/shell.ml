type t = { before : string; after : string }
type early = Kept | Lost
type error = No_root of string | Filled_root of string | No_head

let parse ~root early page =
  let rec find pattern from =
    match from + String.length pattern > String.length page with
    | true -> None
    | false -> (
        match String.sub page from (String.length pattern) = pattern with
        | true -> Some from
        | false -> find pattern (from + 1))
  in
  let rec tag_end at quote =
    match (at < String.length page, quote) with
    | false, _ -> None
    | true, None -> (
        match page.[at] with
        | '>' -> Some at
        | ('"' | '\'') as opened -> tag_end (at + 1) (Some opened)
        | _ -> tag_end (at + 1) None)
    | true, Some opened -> tag_end (at + 1) (match page.[at] = opened with true -> None | false -> quote)
  in
  let names_root first last =
    List.exists
      (fun quote ->
        List.exists
          (fun space -> Option.fold ~none:false ~some:(fun at -> at < last) (find (space ^ "id=" ^ quote ^ root ^ quote) first))
          [ " "; "\t"; "\n"; "\r" ])
      [ "\""; "'" ]
  in
  let rec start_tag from =
    Option.bind (String.index_from_opt page from '<') (fun opening ->
        match find "<!--" opening = Some opening with
        | true -> Option.bind (find "-->" (opening + 4)) (fun closing -> start_tag (closing + 3))
        | false ->
            Option.bind (tag_end (opening + 1) None) (fun closing ->
                match names_root opening closing with true -> Some closing | false -> start_tag (closing + 1)))
  in
  let between first last = String.sub page first (last - first) in
  Result.bind
    (Option.fold ~none:(Error (`Shell (No_root root)))
       ~some:(fun closing ->
         match find "</" (closing + 1) = Some (closing + 1) with
         | true -> Ok (closing + 1)
         | false -> Error (`Shell (Filled_root root)))
       (start_tag 0))
    (fun inside ->
      let after = between inside (String.length page) in
      match (early, find "</head>" 0) with
      | Lost, _ -> Ok { before = between 0 inside; after }
      | Kept, Some head when head < inside ->
          Ok { before = between 0 head ^ "<script>" ^ Melhole_server.capture ^ "</script>" ^ between head inside; after }
      | Kept, (Some _ | None) -> Error (`Shell No_head))

let fill { before; after } html = before ^ html ^ after
