open Ppxlib
open Ast_builder.Default

(** A piece of an interpolated string. *)
type segment = Text of string | Variable of longident

let expanded items =
  let segments text =
    let length = String.length text in
    let identifier character =
      match character with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true | _ -> false
    in
    let variable name =
      match (String.trim name, String.for_all identifier name) with
      | "", _ | _, false -> None
      | _, true -> ( match Longident.parse name with path -> Some (Variable path) | exception _ -> None)
    in
    let rec scan index text_so_far found =
      let flushed = match text_so_far with "" -> found | text -> Text text :: found in
      match index < length with
      | false -> Some (List.rev flushed)
      | true -> (
          match (text.[index], index + 1 < length) with
          | '\\', true -> scan (index + 2) (text_so_far ^ String.sub text index 2) found
          | ('\\' | '$'), false -> None
          | '$', true -> (
              match text.[index + 1] with
              | '(' ->
                  Option.bind (String.index_from_opt text (index + 2) ')') (fun close ->
                      Option.bind (variable (String.sub text (index + 2) (close - index - 2))) (fun segment ->
                          scan (close + 1) "" (segment :: flushed)))
              | 'a' .. 'z' | '_' ->
                  let rec last position =
                    match position < length && identifier text.[position] with true -> last (position + 1) | false -> position
                  in
                  let finish = last (index + 1) in
                  Option.bind (variable (String.sub text (index + 1) (finish - index - 1))) (fun segment ->
                      scan finish "" (segment :: flushed))
              | _ -> None)
          | character, _ -> scan (index + 1) (text_so_far ^ String.make 1 character) found)
    in
    scan 0 "" []
  in
  (object
     inherit Ast_traverse.map as super

     method! expression expression =
       match expression.pexp_desc with
       | Pexp_constant (Pconst_string (text, loc, Some "j")) ->
           Option.fold ~none:expression
             ~some:(fun segments ->
               let piece segment =
                 match segment with
                 | Text text -> pexp_constant ~loc (Pconst_string (text, loc, Some "js"))
                 | Variable path -> pexp_ident ~loc { loc; txt = path }
               in
               let joined =
                 match segments with
                 | [] -> pexp_constant ~loc (Pconst_string ("", loc, Some "js"))
                 | first :: rest ->
                     List.fold_left (fun joined segment -> [%expr Stdlib.( ^ ) [%e joined] [%e piece segment]]) (piece first) rest
               in
               { joined with pexp_attributes = expression.pexp_attributes @ joined.pexp_attributes })
             (segments text)
       | _ -> super#expression expression
  end)
    #structure
    items
