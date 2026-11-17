open Ppxlib

(** What the compiler knows of an element: its tag, if known, and its classes, if they are a
    constant. *)
type element = { tag : string option; classes : string list option }

(** The selectors of a module's declared stylesheets, or none declared. *)
type t = Declared of (string * string) list | Undeclared

(** A compound selector: its tag, classes and pseudo-classes. *)
type compound = { named : string option; dotted : string list; pseudos : string list }

(** How two compound selectors are joined. *)
type combinator = Descendant | Child | Next | Later

let read structure =
  let files =
    List.concat_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_attribute { attr_name = { txt = "meltea.styles"; _ }; attr_payload = PStr payload; attr_loc } ->
            List.concat_map
              (fun (inner : structure_item) ->
                let rec strings (expression : expression) =
                  match expression.pexp_desc with
                  | Pexp_constant (Pconst_string (file, _, _)) -> [ file ]
                  | Pexp_apply (first, rest) -> strings first @ List.concat_map (fun (_, argument) -> strings argument) rest
                  | _ -> []
                in
                match inner.pstr_desc with
                | Pstr_eval (expression, _) ->
                    List.map
                      (fun file -> Filename.concat (Filename.dirname attr_loc.loc_start.pos_fname) file)
                      (strings expression)
                | _ -> [])
              payload
        | _ -> [])
      structure
  in
  match files with
  | [] -> Undeclared
  | _ :: _ ->
      Declared
        (List.concat_map
           (fun file ->
             let text = In_channel.with_open_bin file In_channel.input_all in
             let buffer = Buffer.create (String.length text) in
             let length = String.length text in
             let rec strip at =
               match at < length with
               | false -> ()
               | true -> (
                   match at + 1 < length && text.[at] = '/' && text.[at + 1] = '*' with
                   | true ->
                       let rec close at =
                         match at + 1 < length with
                         | false -> length
                         | true -> ( match text.[at] = '*' && text.[at + 1] = '/' with true -> at + 2 | false -> close (at + 1))
                       in
                       strip (close (at + 2))
                   | false ->
                       Buffer.add_char buffer text.[at];
                       strip (at + 1))
             in
             strip 0;
             let text = Buffer.contents buffer in
             let found = ref [] and start = ref 0 and depth = ref 0 in
             String.iteri
               (fun at c ->
                 match c with
                 | '{' ->
                     let prelude = String.trim (String.sub text !start (at - !start)) in
                     (match (String.length prelude > 0 && prelude.[0] = '@', !depth) with
                     | true, _ -> ()
                     | false, _ -> found := (file, prelude) :: !found);
                     (match String.length prelude > 0 && prelude.[0] = '@' with true -> () | false -> incr depth);
                     start := at + 1
                 | '}' ->
                     (match !depth > 0 with true -> decr depth | false -> ());
                     start := at + 1
                 | ';' when !depth = 0 -> start := at + 1
                 | _ -> ())
               text;
             List.concat_map
               (fun (file, prelude) ->
                 let parts = ref [] and level = ref 0 and from = ref 0 in
                 String.iteri
                   (fun at c ->
                     match c with
                     | '(' -> incr level
                     | ')' -> decr level
                     | ',' when !level = 0 ->
                         parts := String.trim (String.sub prelude !from (at - !from)) :: !parts;
                         from := at + 1
                     | _ -> ())
                   prelude;
                 List.rev_map
                   (fun selector -> (file, selector))
                   (String.trim (String.sub prelude !from (String.length prelude - !from)) :: !parts))
               (List.rev !found))
           files)

let breaking styles ~container ~row =
  let parse selector =
    let length = String.length selector in
    let word at =
      let rec stop at =
        match at < length with
        | false -> at
        | true -> (
            match selector.[at] with
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> stop (at + 1)
            | _ -> at)
      in
      let ended = stop at in
      (String.sub selector at (ended - at), ended)
    in
    let rec skip_paren at level =
      match at < length with
      | false -> at
      | true -> (
          match selector.[at] with
          | '(' -> skip_paren (at + 1) (level + 1)
          | ')' when level = 1 -> at + 1
          | ')' -> skip_paren (at + 1) (level - 1)
          | _ -> skip_paren (at + 1) level)
    in
    let rec compound at current =
      match at < length with
      | false -> (current, at)
      | true -> (
          match selector.[at] with
          | '.' ->
              let name, next = word (at + 1) in
              compound next { current with dotted = name :: current.dotted }
          | '#' ->
              let _, next = word (at + 1) in
              compound next current
          | ':' ->
              let start = match at + 1 < length && selector.[at + 1] = ':' with true -> at + 2 | false -> at + 1 in
              let name, next = word start in
              let next = match next < length && selector.[next] = '(' with true -> skip_paren next 0 | false -> next in
              compound next { current with pseudos = name :: current.pseudos }
          | '[' ->
              let rec close at = match at < length && selector.[at] <> ']' with true -> close (at + 1) | false -> at + 1 in
              compound (close at) current
          | '*' -> compound (at + 1) current
          | 'a' .. 'z' | 'A' .. 'Z' when Option.is_none current.named && current.dotted = [] && current.pseudos = [] ->
              let name, next = word at in
              compound next { current with named = Some (String.lowercase_ascii name) }
          | _ -> (current, at))
    in
    let rec chain at found pending =
      let rec spaces at = match at < length && selector.[at] = ' ' with true -> spaces (at + 1) | false -> at in
      let at = spaces at in
      match at < length with
      | false -> List.rev found
      | true -> (
          match selector.[at] with
          | '>' -> chain (at + 1) found Child
          | '+' -> chain (at + 1) found Next
          | '~' -> chain (at + 1) found Later
          | _ ->
              let part, next = compound at { named = None; dotted = []; pseudos = [] } in
              let next = match next = at with true -> at + 1 | false -> next in
              chain next ((pending, part) :: found) Descendant)
    in
    chain 0 [] Descendant
  in
  let could (element : element) part =
    Option.fold ~none:true ~some:(fun named -> Option.fold ~none:true ~some:(String.equal named) element.tag) part.named
    && Option.fold ~none:true ~some:(fun classes -> List.for_all (fun dotted -> List.mem dotted classes) part.dotted) element.classes
  in
  let counting =
    [ "first-child"; "last-child"; "only-child"; "nth-child"; "nth-last-child"; "first-of-type"; "last-of-type"; "only-of-type"; "nth-of-type"; "nth-last-of-type" ]
  in
  match styles with
  | Undeclared -> None
  | Declared selectors ->
      List.find_opt
        (fun (_, selector) ->
          let parts = parse selector in
          let rec risky previous parts =
            match parts with
            | [] -> false
            | (joined, part) :: rest ->
                (could row part
                && (List.exists (fun pseudo -> List.mem pseudo counting) part.pseudos
                   || (match (joined, previous) with
                      | Child, Some before -> could container before
                      | (Next | Later), Some before -> could row before
                      | (Descendant | Child | Next | Later), _ -> false)))
                || risky (Some part) rest
          in
          risky None parts)
        selectors
