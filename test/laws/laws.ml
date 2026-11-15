open QCheck2
open Melhole_compiler

(** A template node as an HTML parser builds it, plus content inserted at a position. *)
type parsed =
  | Parsed_element of string * (string * Markup.static) list * parsed list
  | Parsed_text of string
  | Parsed_comment
  | Parsed_slot of int

(** What the DOM must show: elements, merged text and slot content. *)
type shown =
  | Shown_element of string * (string * Markup.static) list * shown list
  | Shown_text of string
  | Shown_slot of int

let reconcile_keys = Gen.(array_size (int_bound 14) (map string_of_int (int_bound 5)))
let reconcile_input = Gen.pair reconcile_keys reconcile_keys
let print_keys = Print.(pair (array string) (array string))

let reused (plan : Reconcile.t) =
  List.concat
    (Array.to_list
       (Array.mapi
          (fun j (step : Reconcile.step) ->
            match Reconcile.view step with Keep i | Move i -> [ (j, i) ] | Create -> [])
          plan.steps))

let count key keys =
  Array.fold_left (fun total other -> if String.equal other key then total + 1 else total) 0 keys

let longest_increasing values =
  let values = Array.of_list values in
  let best = Array.make (Array.length values) 1 in
  Array.iteri
    (fun i value ->
      for k = 0 to i - 1 do
        if values.(k) < value then best.(i) <- max best.(i) (best.(k) + 1)
      done)
    values;
  Array.fold_left max 0 best

let simulate old next (plan : Reconcile.t) =
  let dom =
    ref
      (List.filter_map
         (fun i -> if Array.mem i plan.dropped then None else Some (`Old i))
         (List.init (Array.length old) Fun.id))
  in
  let place node anchor =
    let rest = List.filter (fun other -> other <> node) !dom in
    dom :=
      Option.fold ~none:(rest @ [ node ])
        ~some:(fun anchor ->
          List.concat_map (fun other -> if other = anchor then [ node; other ] else [ other ]) rest)
        anchor
  in
  let rec walk j anchor =
    if j >= 0 then begin
      let node = match Reconcile.view plan.steps.(j) with Create -> `New j | Keep i | Move i -> `Old i in
      (match Reconcile.view plan.steps.(j) with Create | Move _ -> place node anchor | Keep _ -> ());
      walk (j - 1) (Some node)
    end
  in
  walk (Array.length next - 1) None;
  !dom

let edited_input =
  Gen.(
    let distinct = map (fun count -> Array.init count (fun index -> string_of_int (index * 7 mod 41))) (int_bound 30) in
    let edit keys =
      let count = Array.length keys in
      let at bound = if bound <= 0 then return 0 else int_bound (bound - 1) in
      int_bound 8 >>= fun choice ->
      at count >>= fun first ->
      at count >>= fun second ->
      match (choice, count) with
      | _, 0 -> return [| "n" |]
      | 0, _ ->
          let next = Array.copy keys in
          next.(0) <- keys.(count - 1);
          next.(count - 1) <- keys.(0);
          return next
      | 1, _ ->
          let next = Array.copy keys in
          next.(first) <- keys.(second);
          next.(second) <- keys.(first);
          return next
      | 2, _ -> return (Array.of_list (List.filteri (fun index _ -> index <> first) (Array.to_list keys)))
      | 3, _ ->
          return (Array.concat [ Array.sub keys 0 first; [| "n" ^ string_of_int first |]; Array.sub keys first (count - first) ])
      | 4, _ ->
          let low = min first second and high = max first second in
          return (Array.mapi (fun index key -> if index >= low && index <= high then keys.(high - (index - low)) else key) keys)
      | 5, _ ->
          let moved = keys.(first) in
          let rest = List.filteri (fun index _ -> index <> first) (Array.to_list keys) in
          return (Array.of_list (List.filteri (fun index _ -> index < second) rest @ [ moved ] @ List.filteri (fun index _ -> index >= second) rest))
      | 6, _ -> return [||]
      | _, _ -> return (Array.map (fun key -> "r" ^ key) keys)
    in
    distinct >>= fun keys ->
    int_range 1 3 >>= fun times ->
    let rec apply remaining current = if remaining = 0 then return current else edit current >>= apply (remaining - 1) in
    apply times keys >>= fun next -> return (keys, next))

let reconcile_laws (kind, input) =
  [
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "one step per new entry") ~print:print_keys input
      (fun (old, next) -> Array.length (Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next)).steps = Array.length next);
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "reused and dropped entries partition the old ones")
      ~print:print_keys input (fun (old, next) ->
        let plan = Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next) in
        List.sort compare (List.map snd (reused plan) @ Array.to_list plan.dropped)
        = List.init (Array.length old) Fun.id);
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "a reused entry has the same key") ~print:print_keys
      input (fun (old, next) ->
        List.for_all
          (fun (j, i) -> String.equal old.(i) next.(j))
          (reused (Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next))));
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "reuse is maximal, the sum over keys of min(old, new) counts")
      ~print:print_keys input (fun (old, next) ->
        let keys = List.sort_uniq compare (Array.to_list old @ Array.to_list next) in
        List.length (reused (Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next)))
        = List.fold_left (fun total key -> total + min (count key old) (count key next)) 0 keys);
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "kept entries appear in increasing old order") ~print:print_keys
      input (fun (old, next) ->
        let kept =
          List.concat_map
            (fun (step : Reconcile.step) -> match Reconcile.view step with Keep i -> [ i ] | Move _ | Create -> [])
            (Array.to_list (Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next)).steps)
        in
        kept = List.sort_uniq compare kept);
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "kept entries are a longest increasing subsequence (moves are minimal)")
      ~print:print_keys input (fun (old, next) ->
        let plan = Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next) in
        let kept =
          Array.fold_left
            (fun total (step : Reconcile.step) ->
              match Reconcile.view step with Keep _ -> total + 1 | Move _ | Create -> total)
            0 plan.steps
        in
        kept = longest_increasing (List.map snd (reused plan)));
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "executing the plan right to left yields exactly the new sequence")
      ~print:print_keys input (fun (old, next) ->
        let plan = Reconcile.plan ~old:(Reconcile.string_keys old) ~next:(Reconcile.string_keys next) in
        simulate old next plan
        = Array.to_list
            (Array.mapi
               (fun j (step : Reconcile.step) ->
                 match Reconcile.view step with Create -> `New j | Keep i | Move i -> `Old i)
               plan.steps));
    Test.make ~count:2000 ~name:("Reconcile, " ^ kind ^ ": " ^ "an unchanged sequence keeps every entry in place")
      ~print:print_keys input (fun (keys, _) ->
        let plan = Reconcile.plan ~old:(Reconcile.string_keys keys) ~next:(Reconcile.string_keys keys) in
        plan.dropped = [||]
        && Array.for_all2
             (fun (step : Reconcile.step) i -> Reconcile.view step = Keep i)
             plan.steps
             (Array.init (Array.length keys) Fun.id));
  ]

let text_gen =
  Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; ' '; '&'; '<'; '>'; '"'; '\''; ';'; '#' ]) (int_bound 3))

let tree_gen =
  let open Gen in
  let site = oneof_list [ Markup.Attr "class"; Prop "value"; Handler "click"; Ref; Style ] in
  let static =
    pair (oneof_list [ "title"; "lang"; "hidden"; "dir" ])
      (oneof [ map (fun text -> Markup.Valued text) text_gen; pure Markup.Bare ])
  in
  let leaf =
    [
      map (fun text -> Markup.Text text) text_gen;
      map (fun slot -> Markup.Slot (slot, 0)) (oneof_list [ Markup.Text_slot; Child_slot ]);
    ]
  in
  fix
    (fun self depth ->
      let* tag = oneof_list [ "div"; "span"; "b"; "em"; "section"; "br"; "input" ] in
      let* statics = list_size (int_bound 2) static in
      let* dynamics = list_size (int_bound 2) (map (fun site -> (site, 0)) site) in
      let* children =
        match (Markup.void tag, depth) with
        | `Void, _ -> pure []
        | `Container, 0 -> list_size (int_bound 5) (oneof leaf)
        | `Container, _ ->
            list_size (int_bound 5)
              (oneof (map (fun element -> Markup.Element element) (self (depth - 1)) :: leaf))
      in
      pure { Markup.tag; statics; dynamics; children })
    2

let number root =
  let last = ref 0 in
  let fresh () =
    incr last;
    !last
  in
  let rec element (tree : int Markup.element) =
    let id = fresh () in
    {
      tree with
      statics =
        ("data-n", Markup.Valued (string_of_int id))
        :: List.sort_uniq (fun (a, _) (b, _) -> compare a b) tree.statics;
      dynamics = List.map (fun (site, _) -> (site, id)) tree.dynamics;
      children =
        List.map
          (fun (node : int Markup.node) ->
            match node with
            | Element child -> Markup.Element (element child)
            | Text text -> Text text
            | Slot (slot, _) -> Slot (slot, fresh ()))
          tree.children;
    }
  in
  element root

let parse html =
  let length = String.length html in
  let starts prefix at =
    at + String.length prefix <= length && String.sub html at (String.length prefix) = prefix
  in
  let decode start stop =
    let buffer = Buffer.create (stop - start) in
    let rec go at =
      if at < stop then
        match
          List.find_opt
            (fun (name, _) -> starts name at)
            [ ("&amp;", '&'); ("&lt;", '<'); ("&gt;", '>'); ("&quot;", '"') ]
        with
        | Some (name, character) ->
            Buffer.add_char buffer character;
            go (at + String.length name)
        | None ->
            Buffer.add_char buffer html.[at];
            go (at + 1)
    in
    go start;
    Buffer.contents buffer
  in
  let rec scan stops at =
    if at >= length || String.contains stops html.[at] then at else scan stops (at + 1)
  in
  let rec attributes at collected =
    match html.[at] with
    | '>' -> (List.rev collected, at + 1)
    | ' ' -> attributes (at + 1) collected
    | _ -> (
        let stop = scan "= >" at in
        let name = String.sub html at (stop - at) in
        match html.[stop] with
        | '=' ->
            let close = scan "\"" (stop + 2) in
            attributes (close + 1) ((name, Markup.Valued (decode (stop + 2) close)) :: collected)
        | _ -> attributes stop ((name, Markup.Bare) :: collected))
  and children at collected =
    if at >= length then (List.rev collected, at)
    else if starts "<!---->" at then children (at + 7) (Parsed_comment :: collected)
    else if starts "</" at then (List.rev collected, scan ">" at + 1)
    else if html.[at] = '<' then
      let node, after = element at in
      children after (node :: collected)
    else
      let stop = scan "<" at in
      let content = decode at stop in
      match collected with
      | Parsed_text previous :: rest -> children stop (Parsed_text (previous ^ content) :: rest)
      | _ -> children stop (Parsed_text content :: collected)
  and element at =
    let stop = scan " >" (at + 1) in
    let tag = String.sub html (at + 1) (stop - at - 1) in
    let statics, after = attributes stop [] in
    match Markup.void tag with
    | `Void -> (Parsed_element (tag, statics, []), after)
    | `Container ->
        let nodes, after = children after [] in
        (Parsed_element (tag, statics, nodes), after)
  in
  fst (element 0)

let rec merge shown =
  match shown with
  | Shown_text a :: Shown_text b :: rest -> merge (Shown_text (a ^ b) :: rest)
  | node :: rest -> node :: merge rest
  | [] -> []

let rec shown_source (tree : int Markup.element) =
  Shown_element
    ( tree.tag,
      tree.statics,
      merge
        (List.concat_map
           (fun (node : int Markup.node) ->
             match node with
             | Element child -> [ shown_source child ]
             | Text "" -> []
             | Text text -> [ Shown_text text ]
             | Slot (_, slot) -> [ Shown_slot slot ])
           tree.children) )

let rec shown_parsed node =
  match node with
  | Parsed_element (tag, statics, children) ->
      [ Shown_element (tag, statics, merge (List.concat_map shown_parsed children)) ]
  | Parsed_text text -> [ Shown_text text ]
  | Parsed_comment -> []
  | Parsed_slot slot -> [ Shown_slot slot ]

let rec resolve node path =
  match (path, node) with
  | [], _ -> Some node
  | index :: rest, Parsed_element (_, _, children) ->
      Option.bind (List.nth_opt children index) (fun child -> resolve child rest)
  | _ :: _, (Parsed_text _ | Parsed_comment | Parsed_slot _) -> None

let positions holes =
  List.concat_map
    (fun (hole : int Markup.hole) ->
      match hole with
      | At_position (path, index, _, slot) -> [ (path, index, slot) ]
      | On_element (path, Sole_text, slot) -> [ (path, 0, slot) ]
      | On_element (_, (Attr _ | Prop _ | Control _ | Handler _ | Ref | Style), _) -> [])
    holes

let rec insert positions path node =
  match node with
  | Parsed_element (tag, statics, children) ->
      let slots_at index =
        List.filter_map
          (fun (at, position, slot) ->
            if at = path && position = index then Some (Parsed_slot slot) else None)
          positions
      in
      let children = List.mapi (fun index child -> insert positions (path @ [ index ]) child) children in
      Parsed_element
        ( tag,
          statics,
          List.concat (List.mapi (fun index child -> slots_at index @ [ child ]) children)
          @ slots_at (List.length children) )
  | Parsed_text _ | Parsed_comment | Parsed_slot _ -> node

let print_tree tree =
  let html, holes = Markup.render (number tree) in
  let path indices = "[" ^ String.concat ";" (List.map string_of_int indices) ^ "]" in
  html ^ "\n"
  ^ String.concat "\n"
      (List.map
         (fun (hole : int Markup.hole) ->
           match hole with
           | On_element (at, _, id) -> Printf.sprintf "  element %s carries #%d" (path at) id
           | At_position (at, index, _, slot) -> Printf.sprintf "  slot %d at %s before %d" slot (path at) index)
         holes)

let markup_laws =
  [
    Test.make ~count:2000 ~name:"Markup: every element hole resolves to the element that carries it"
      ~print:print_tree tree_gen (fun tree ->
        let tree = number tree in
        let html, holes = Markup.render tree in
        let root = parse html in
        List.for_all
          (fun (hole : int Markup.hole) ->
            match hole with
            | On_element (path, Sole_text, _) -> (
                match resolve root path with
                | Some (Parsed_element (_, _, [])) -> true
                | Some (Parsed_element (_, _, _ :: _) | Parsed_text _ | Parsed_comment | Parsed_slot _) | None -> false)
            | On_element (path, _, id) -> (
                match resolve root path with
                | Some (Parsed_element (_, statics, _)) ->
                    List.assoc_opt "data-n" statics = Some (Markup.Valued (string_of_int id))
                | Some (Parsed_text _ | Parsed_comment | Parsed_slot _) | None -> false)
            | At_position _ -> true)
          holes);
    Test.make ~count:2000 ~name:"Markup: position holes in one parent never share an anchor"
      ~print:print_tree tree_gen (fun tree ->
        let anchors =
          List.map (fun (path, index, _) -> (path, index)) (positions (snd (Markup.render (number tree))))
        in
        List.length anchors = List.length (List.sort_uniq compare anchors));
    Test.make ~count:2000
      ~name:"Markup: the compiled walk reaches every hole's node through firstChild and nextSibling"
      ~print:print_tree tree_gen (fun tree ->
        let tree = number tree in
        let html, holes = Markup.render tree in
        let root = parse html in
        let exists handle = Option.is_some (resolve root handle) in
        let first handle = if exists (handle @ [ 0 ]) then Some (handle @ [ 0 ]) else None in
        let next handle =
          match List.rev handle with
          | index :: parent when exists (List.rev ((index + 1) :: parent)) ->
              Some (List.rev ((index + 1) :: parent))
          | _ -> None
        in
        let reached =
          List.fold_left
            (fun reached (indices, (step : Markup.step)) ->
              Option.bind reached (fun reached ->
                  let source = match step with First_of source | Next_of source -> source in
                  Option.bind (List.assoc_opt source reached) (fun handle ->
                      Option.map
                        (fun found -> (indices, found) :: reached)
                        (match step with First_of _ -> first handle | Next_of _ -> next handle))))
            (Some [ ([], []) ]) (Markup.walk holes)
        in
        Option.fold ~none:false
          ~some:(fun reached ->
            List.for_all
              (fun (hole : int Markup.hole) ->
                match hole with
                | On_element (path, Sole_text, _) -> (
                    match Option.bind (List.assoc_opt path reached) (resolve root) with
                    | Some (Parsed_element (_, _, [])) -> true
                    | Some (Parsed_element (_, _, _ :: _) | Parsed_text _ | Parsed_comment | Parsed_slot _) | None -> false)
                | On_element (path, _, id) -> (
                    match Option.bind (List.assoc_opt path reached) (resolve root) with
                    | Some (Parsed_element (_, statics, _)) ->
                        List.assoc_opt "data-n" statics = Some (Markup.Valued (string_of_int id))
                    | Some (Parsed_text _ | Parsed_comment | Parsed_slot _) | None -> false)
                | At_position (path, index, _, _) -> (
                    match List.assoc_opt path reached with
                    | None -> false
                    | Some parent ->
                        let expected = if exists (parent @ [ index ]) then Some (parent @ [ index ]) else None in
                        let anchored =
                          match Markup.anchor path index with
                          | First_or_end at -> Option.map (fun handle -> first handle) (List.assoc_opt at reached)
                          | Next_or_end at -> Option.map (fun handle -> next handle) (List.assoc_opt at reached)
                        in
                        anchored = Some expected))
              holes)
          reached);
    Test.make
      ~name:"Markup: parsing the HTML and inserting every slot at its position rebuilds the source"
      ~print:print_tree tree_gen (fun tree ->
        let tree = number tree in
        let html, holes = Markup.render tree in
        shown_parsed (insert (positions holes) [] (parse html)) = [ shown_source tree ]);
  ]

let dom_props_laws =
  let all = Dom_props.Prop.all in
  let labels = List.map Dom_props.Prop.to_string all in
  let well_formed name =
    String.length name > 0
    && (match name.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
    && String.for_all
         (fun character ->
           match character with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | ':' -> true | _ -> false)
         name
  in
  [
    Test.make ~count:1 ~name:"Dom_props: of_string inverts to_string on every prop" Gen.unit
      (fun () ->
        List.for_all (fun prop -> Dom_props.Prop.of_string (Dom_props.Prop.to_string prop) = Some prop) all);
    Test.make ~count:1 ~name:"Dom_props: to_string is injective" Gen.unit (fun () ->
        List.length (List.sort_uniq compare labels) = List.length all);
    Test.make ~count:2000 ~name:"Dom_props: of_string only accepts the exact label" ~print:Print.string
      Gen.(
        oneof
          [
            oneof_list labels;
            map String.lowercase_ascii (oneof_list labels);
            map (fun label -> label ^ "x") (oneof_list labels);
            string_size ~gen:printable (int_bound 12);
          ])
      (fun label ->
        Option.fold ~none:true
          ~some:(fun prop -> String.equal (Dom_props.Prop.to_string prop) label)
          (Dom_props.Prop.of_string label));
    Test.make ~count:1 ~name:"Dom_props: attribute and event names are well formed" Gen.unit
      (fun () ->
        List.for_all
          (fun prop ->
            let label = Dom_props.Prop.to_string prop in
            match Dom_props.kind prop with
            | Attribute (name, _) | Property (name, _) | Controlled (name, _) ->
                well_formed name
                && (String.length label < 5
                   || (not (String.sub label 0 4 = "aria" && Char.uppercase_ascii label.[4] = label.[4]))
                   || String.equal name ("aria-" ^ String.lowercase_ascii (String.sub label 4 (String.length label - 4))))
            | Handler event -> String.for_all (fun character -> character >= 'a' && character <= 'z') event
            | Style | Ref | Inner_html | Ignored -> true)
          all);
  ]

let hook_name_laws =
  let name =
    Gen.(
      map2 ( ^ )
        (oneof_list [ "use"; "use"; "us"; "user"; "Use"; "used"; "" ])
        (string_size ~gen:(oneof_list [ 'a'; 'z'; 'A'; 'Z'; '_'; '\''; '0'; '7'; 'x' ]) (int_bound 5)))
  in
  [
    Test.make ~count:3000 ~name:"Hook_name: a name is a hook exactly when it is use, or use then no lowercase letter"
      ~print:Print.string name (fun name ->
        let expected =
          String.equal name "use"
          || String.length name > 3
             && String.equal (String.sub name 0 3) "use"
             && not (name.[3] >= 'a' && name.[3] <= 'z')
        in
        match Melhole_compiler.Hook_name.of_name name with Hook _ -> expected | Plain -> not expected);
    Test.make ~count:3000 ~name:"Hook_name: a hook's base and dependencies spell its name, the dependencies all digits"
      ~print:Print.string name (fun name ->
        match Melhole_compiler.Hook_name.of_name name with
        | Plain -> true
        | Hook { base; dependencies } ->
            let digit character = character >= '0' && character <= '9' in
            String.equal (base ^ dependencies) name
            && String.for_all digit dependencies
            && (String.equal base "" || not (digit base.[String.length base - 1])));
  ]

(** What one handler of a generated view does. *)
type sends =
  | On_input of int  (** Every input event sends this message. *)
  | On_key of int * int  (** Enter sends the first, any other key the second. *)
  | On_click of int  (** A click, which is not a frequent event. *)

(** A generated module: fields each message writes, what each local and list reads, the handlers. *)
type program = {
  writes : int list array;
  locals : (int list * int list) list;  (** Fields and earlier locals each local reads. *)
  lists : (int list * int list) list;
  handlers : sends list;
}

let fanout_program =
  let open Gen in
  let subset bound = map (fun bits -> List.filter (fun index -> bits land (1 lsl index) <> 0) (List.init bound Fun.id)) (int_bound ((1 lsl bound) - 1)) in
  let* writes = array_size (pure 4) (subset 5) in
  let* count = int_bound 3 in
  let* locals = flatten_list (List.init count (fun index -> pair (subset 5) (subset index))) in
  let* lists = list_size (int_range 1 2) (pair (subset 5) (subset count)) in
  let* handlers =
    list_size (int_bound 4)
      (oneof
         [
           map (fun message -> On_input message) (int_bound 3);
           map2 (fun enter other -> On_key (enter, other)) (int_bound 3) (int_bound 3);
           map (fun message -> On_click message) (int_bound 3);
         ])
  in
  pure { writes; locals; lists; handlers }

let fanout_source program =
  let reads (fields, locals) =
    match List.map (Printf.sprintf "model.f%d") fields @ List.map (Printf.sprintf "l%d") locals with
    | [] -> "()"
    | parts -> "(" ^ String.concat ", " parts ^ ", ())"
  in
  let update =
    "let update message model =\n  match message with\n"
    ^ String.concat ""
        (Array.to_list
           (Array.mapi
              (fun index written ->
                match written with
                | [] -> Printf.sprintf "  | M%d -> model\n" index
                | _ ->
                    Printf.sprintf "  | M%d -> { model with %s }\n" index
                      (String.concat "; " (List.map (fun field -> Printf.sprintf "f%d = model.f%d + 1" field field) written)))
              program.writes))
  in
  let lines =
    [ "let view model =" ]
    @ List.mapi (fun index local -> Printf.sprintf "  let l%d = %s in" index (reads local)) program.locals
    @ [ "  ((div ~children:[" ]
    @ List.map (fun list -> Printf.sprintf "    Html.list (List.map row %s);" (reads list)) program.lists
    @ List.map
        (fun handler ->
          match handler with
          | On_input message -> Printf.sprintf "    ((input ~onInput:(fun text -> M%d) ()) [@JSX]);" message
          | On_key (enter, other) ->
              Printf.sprintf "    ((input ~onKeyDown:(fun key -> match key with \"Enter\" -> M%d | _ -> M%d) ()) [@JSX]);" enter other
          | On_click message -> Printf.sprintf "    ((button ~onClick:M%d ()) [@JSX]);" message)
        program.handlers
    @ [ "  ] ()) [@JSX])" ]
  in
  let header = String.split_on_char '\n' update in
  (update ^ String.concat "\n" lines, List.length header)

let fanout_expected program =
  let rec closure (fields, locals) =
    List.sort_uniq compare (fields @ List.concat_map (fun local -> closure (List.nth program.locals local)) locals)
  in
  let reached message =
    List.exists
      (fun list -> List.exists (fun field -> List.mem field (closure list)) program.writes.(message))
      program.lists
  in
  List.filter_map Fun.id
    (List.mapi
       (fun index handler ->
         let sends = match handler with On_input message -> [ message ] | On_key (_, other) -> [ other ] | On_click _ -> [] in
         match List.exists reached sends with true -> Some index | false -> None)
       program.handlers)

let fanout_alerted program =
  let source, first = fanout_source program in
  let lexbuf = Lexing.from_string source in
  let items = Ppxlib.Parse.implementation lexbuf in
  let handler_line = first + List.length program.locals + 1 + List.length program.lists + 1 in
  let found =
    object
      inherit [int list] Ppxlib.Ast_traverse.fold as super

      method! expression expression lines =
        match expression.pexp_desc with
        | Pexp_letmodule ({ txt = Some "Melhole_fanout"; _ }, _, _) ->
            super#expression expression ((expression.pexp_loc.loc_start.pos_lnum - handler_line) :: lines)
        | _ -> super#expression expression lines
    end
  in
  List.sort_uniq compare (found#structure (Meltea_ppx.Fanout.annotate items) [])

let fanout_laws =
  [
    Test.make ~name:"Fanout: an alert stands at exactly the frequent handlers whose messages write a field a list reads"
      ~count:500 fanout_program (fun program -> fanout_alerted program = fanout_expected program);
  ]

(** A page around an empty root, in parts, so that the expected output can be spelled out. *)
type shell_page = {
  head : string;  (** Markup in the head. *)
  body : string;  (** From [<body>] to the end of the root's start tag. *)
  closing : string;  (** The root's end tag and everything after it. *)
  root : string;  (** The root's id. *)
}

let shell_gen =
  let open Gen in
  let snippets =
    [ "<p>hi</p>"; "<!-- id=\"main\" -->"; " "; "\n"; "<div id=\"other\"></div>"; "<span data-id=\"main\"></span>"; "<i title='a>b'></i>"; "text" ]
  in
  let markup = map (String.concat "") (list_size (int_bound 4) (oneof_list snippets)) in
  let* root = oneof_list [ "main"; "app" ] in
  let* quote = oneof_list [ "\""; "'" ] in
  let* tag = oneof_list [ "div"; "main"; "section" ] in
  let* attributes = oneof_list [ ""; " class=\"x\""; " data-id=\"main\""; " title=\"<b>\"" ] in
  let* head = markup in
  let* before = markup in
  let* after = markup in
  return
    {
      head;
      body = "<body>" ^ before ^ "<" ^ tag ^ attributes ^ " id=" ^ quote ^ root ^ quote ^ attributes ^ ">";
      closing = "</" ^ tag ^ ">" ^ after ^ "</body></html>";
      root;
    }

let shell_laws =
  let opened { head; _ } = "<html><head>" ^ head in
  let whole page = opened page ^ "</head>" ^ page.body ^ page.closing in
  let html = Gen.(string_size ~gen:printable (int_bound 30)) in
  let filled page early html = Result.map (fun shell -> Shell.fill shell html) (Shell.parse ~root:page.root early (whole page)) in
  [
    Test.make ~count:1000 ~name:"Shell: the HTML goes right inside the empty root, and nothing else changes"
      ~print:Print.(pair whole string) (Gen.pair shell_gen html) (fun (page, html) ->
        filled page Shell.Lost html = Ok (opened page ^ "</head>" ^ page.body ^ html ^ page.closing));
    Test.make ~count:1000 ~name:"Shell: with early inputs kept, the capture script also goes right before </head>"
      ~print:Print.(pair whole string) (Gen.pair shell_gen html) (fun (page, html) ->
        filled page Shell.Kept html
        = Ok (opened page ^ "<script>" ^ Melhole_server.capture ^ "</script></head>" ^ page.body ^ html ^ page.closing));
    Test.make ~count:1000 ~name:"Shell: a filled root, a missing root, or no head before the root for the capture script is refused"
      ~print:whole shell_gen (fun page ->
        ( Shell.parse ~root:page.root Shell.Lost (opened page ^ "</head>" ^ page.body ^ "<b></b>" ^ page.closing),
          Shell.parse ~root:"elsewhere" Shell.Lost (whole page),
          Shell.parse ~root:page.root Shell.Kept (page.body ^ page.closing ^ "<head></head>") )
        = (Error (`Shell (Shell.Filled_root page.root)), Error (`Shell (Shell.No_root "elsewhere")), Error (`Shell Shell.No_head)));
  ]

(** What a generated branch writes into one field. *)
type written_value =
  | Same_field  (** [model.f] into [f]. *)
  | Other_field of int  (** [model.g] into [f]. *)
  | Constant of int  (** The constructor [C<n>]. *)
  | Computed  (** [V (model.f0 + 1)]. *)

(** The body of a generated branch of [update]. *)
type written =
  | Direct of (int * written_value) list
  | Helped of (int * written_value) list  (** [{ (helper x) with … }]. *)
  | Chained of (int * written_value) list * (int * written_value) list  (** [let next = { model with … } in { next with … }]. *)
  | Branching of (int * written_value) list * (int * written_value) list  (** [if … then … else …]. *)
  | Unchanged
  | Opaque_call  (** [reset model]: a function the analysis cannot read. *)

(** A generated [update]: its helper's own writes, its branches, and whether it matches a tuple. *)
type lattice_program = { helper : (int * written_value) list; written : written list; scrutinee : [ `Message | `Tuple ] }

let lattice_program =
  let open Gen in
  let writes =
    let* fields = map (fun bits -> List.filter (fun index -> bits land (1 lsl index) <> 0) (List.init 5 Fun.id)) (int_bound 31) in
    flatten_list
      (List.map
         (fun field ->
           map
             (fun value -> (field, value))
             (oneof
                [ pure Same_field; map (fun other -> Other_field ((field + 1 + other) mod 5)) (int_bound 3); map (fun index -> Constant index) (int_bound 2); pure Computed ]))
         fields)
  in
  let* helper = writes in
  let* written =
    list_size (int_range 1 6)
      (oneof
         [
           map (fun writes -> Direct writes) writes;
           map (fun writes -> Helped writes) writes;
           map2 (fun first second -> Chained (first, second)) writes writes;
           map2 (fun first second -> Branching (first, second)) writes writes;
           pure Unchanged;
           pure Opaque_call;
         ])
  in
  let* scrutinee = oneof_list [ `Message; `Tuple ] in
  pure { helper = List.filter (fun (field, _) -> field <> 4) helper; written; scrutinee }

let lattice_source program =
  let value field written =
    match written with
    | Same_field -> Printf.sprintf "model.f%d" field
    | Other_field other -> Printf.sprintf "model.f%d" other
    | Constant index -> Printf.sprintf "C%d" index
    | Computed -> "V (model.f0 + 1)"
  in
  let fields writes = String.concat "; " (List.map (fun (field, written) -> Printf.sprintf "f%d = %s" field (value field written)) writes) in
  let record base writes = match writes with [] -> base | _ :: _ -> Printf.sprintf "{ %s with %s }" base (fields writes) in
  let body written =
    match written with
    | Direct writes -> record "model" writes
    | Helped writes -> record "(helper C2)" writes
    | Chained (first, second) -> Printf.sprintf "let next = %s in %s" (record "model" first) (record "next" second)
    | Branching (first, second) -> Printf.sprintf "if model.f0 = C0 then %s else %s" (record "model" first) (record "model" second)
    | Unchanged -> "model"
    | Opaque_call -> "reset model"
  in
  "let update message model =\n"
  ^ Printf.sprintf "  let helper value = { model with %s } in\n" (fields ((4, Computed) :: program.helper))
  ^ (match program.scrutinee with `Message -> "  match message with\n" | `Tuple -> "  match (message, model.f0) with\n")
  ^ String.concat ""
      (List.mapi
         (fun index written ->
           Printf.sprintf "  | M%d%s -> %s\n" index (match program.scrutinee with `Message -> "" | `Tuple -> ", _") (body written))
         program.written)

(** A field's change as the law expects it. *)
type expected_change = Kept_field | Literal_field of string list | Set_field

let lattice_expected program =
  let change field written =
    match written with
    | Same_field -> Kept_field
    | Other_field other when other = field -> Kept_field
    | Other_field _ | Computed -> Set_field
    | Constant index -> Literal_field [ Printf.sprintf "C%d" index ]
  in
  let over base writes = List.map (fun (field, written) -> (field, change field written)) writes @ List.filter (fun (field, _) -> not (List.mem_assoc field writes)) base in
  let join first second =
    match (first, second) with
    | Set_field, _ | _, Set_field -> Set_field
    | Kept_field, other | other, Kept_field -> other
    | Literal_field one, Literal_field other -> Literal_field (List.sort_uniq compare (one @ other))
  in
  let joined first second =
    List.map
      (fun field ->
        let at changes = Option.value ~default:Kept_field (List.assoc_opt field changes) in
        (field, join (at first) (at second)))
      (List.init 5 Fun.id)
  in
  let branch written =
    match written with
    | Direct writes -> Some (over [] writes)
    | Helped writes -> Some (over (over [] ((4, Computed) :: program.helper)) writes)
    | Chained (first, second) -> Some (over (over [] first) second)
    | Branching (first, second) -> Some (joined (over [] first) (over [] second))
    | Unchanged -> Some []
    | Opaque_call -> None
  in
  let branches = List.map branch program.written in
  let normal changes =
    List.sort compare (List.filter_map (fun (field, change) -> match change with Kept_field -> None | _ -> Some (Printf.sprintf "f%d" field, change)) changes)
  in
  ( List.map (Option.map (fun changes -> List.map fst (normal changes))) branches,
    List.fold_left (fun found branch -> match (found, branch) with Some found, Some branch -> Some (joined found branch) | _, _ -> None) (Some []) branches
    |> Option.map normal )

let lattice_found program =
  let items = Ppxlib.Parse.implementation (Lexing.from_string (lattice_source program)) in
  let update =
    List.find_map
      (fun (item : Ppxlib.structure_item) ->
        match item.pstr_desc with Pstr_value (_, [ binding ]) -> Some binding.pvb_expr | _ -> None)
      items
  in
  let normal (outcome : Meltea_ppx.Fields.outcome) =
    match outcome with
    | Unknown -> None
    | Model changes ->
        Some
          (List.sort compare
             (List.map
                (fun (name, (change : Meltea_ppx.Fields.change)) ->
                  ( name,
                    match change with
                    | Kept -> Kept_field
                    | Literals (first, rest) -> Literal_field (first :: rest)
                    | Set -> Set_field ))
                changes))
  in
  Option.map
    (fun branches ->
      ( List.map
          (fun (branch : Meltea_ppx.Fields.branch) ->
            match Meltea_ppx.Fields.written branch.outcome with Every -> None | Named names -> Some names)
          branches,
        normal (Meltea_ppx.Fields.lattice branches) ))
    (Option.bind update Meltea_ppx.Fields.branches)

let lattice_laws =
  [
    Test.make ~name:"Fields: what each branch of update writes, and the lattice over all of them, agree with a model of the code"
      ~count:1000 ~print:lattice_source lattice_program (fun program -> lattice_found program = Some (lattice_expected program));
  ]

(** What a branch of a generated [update] does. *)
type branch =
  | Leaves  (** [(model, Cmd.none)]. *)
  | Writes  (** Changes a field. *)
  | Commands of int  (** Leaves the model and sends the message through a command. *)
  | Chooses of bool * bool  (** A [match] on a field whose two arms leave the model ([true]) or write it. *)

(** A generated module for the dead-code alerts: cases of [update] (the messages each takes, or
    every remaining one, and its branch), messages [init] sends and the messages each handler
    sends. *)
type dead_program = {
  messages : int;
  cases : (int list option * branch) list;
  started : int list;
  clicks : int list list;
}

let dead_program =
  let open Gen in
  let* messages = int_range 2 5 in
  let message = int_bound (messages - 1) in
  let branch =
    oneof
      [ pure Leaves; pure Writes; map (fun sent -> Commands sent) message; map2 (fun first second -> Chooses (first, second)) bool bool ]
  in
  let* order = shuffle_list (List.init messages Fun.id) in
  let* sizes = list_size (pure messages) (int_range 1 2) in
  let rec grouped order sizes =
    match (order, sizes) with
    | [], _ | _, [] -> []
    | _, size :: sizes ->
        let taken = List.filteri (fun index _ -> index < size) order in
        taken :: grouped (List.filteri (fun index _ -> index >= size) order) sizes
  in
  let groups = grouped order sizes in
  let* rest = int_range 1 (List.length groups) in
  let* catch_all = bool in
  let groups = match catch_all with true -> List.filteri (fun index _ -> index < rest) groups | false -> groups in
  let* named = list_size (pure (List.length groups)) branch in
  let* last = branch in
  let cases = List.map2 (fun group branch -> (Some group, branch)) groups named @ match catch_all with true -> [ (None, last) ] | false -> [] in
  let* started = list_size (int_bound 1) message in
  let* clicks = list_size (int_bound 4) (list_size (int_range 1 2) message) in
  pure { messages; cases; started; clicks }

let dead_source program =
  let constructor index = Printf.sprintf "M%d" index in
  let arm branch =
    match branch with
    | Leaves -> "(model, Cmd.none)"
    | Writes -> "({ model with f = 1 }, Cmd.none)"
    | Commands sent -> Printf.sprintf "(model, Cmd.after 1 %s)" (constructor sent)
    | Chooses (first, second) ->
        let side leaves = match leaves with true -> "(model, Cmd.none)" | false -> "({ model with f = 2 }, Cmd.none)" in
        Printf.sprintf "(match model.f with 0 -> %s | _ -> %s)" (side first) (side second)
  in
  let lines =
    [ "type message =" ]
    @ List.init program.messages (fun index -> "  | " ^ constructor index)
    @ [
        Printf.sprintf "let init = ({ f = 0 }, %s)"
          (match program.started with [] -> "Cmd.none" | sent :: _ -> "Cmd.after 1 " ^ constructor sent);
        "let update message model =";
        "  match message with";
      ]
    @ List.map
        (fun (taken, branch) ->
          Printf.sprintf "  | %s -> %s"
            (Option.fold ~none:"_" ~some:(fun taken -> String.concat " | " (List.map constructor taken)) taken)
            (arm branch))
        program.cases
    @ [ "let view model =";
        "  ((div ~children:[" ]
    @ List.map
        (fun sent ->
          match sent with
          | [ only ] -> Printf.sprintf "    ((button ~onClick:%s ()) [@JSX]);" (constructor only)
          | first :: second :: _ ->
              Printf.sprintf "    ((input ~onKeyDown:(fun key -> match key with \"Enter\" -> %s | _ -> %s) ()) [@JSX]);"
                (constructor first) (constructor second)
          | [] -> "")
        program.clicks
    @ [ "  ] ()) [@JSX])" ]
  in
  String.concat "\n" lines

let dead_expected program =
  let taking message = List.find_opt (fun (taken, _) -> Option.fold ~none:true ~some:(List.mem message) taken) program.cases in
  let sent message =
    List.mem message program.started
    || List.exists (List.mem message) program.clicks
    || List.exists
         (fun (taken, branch) ->
           match branch with
           | Commands target -> target = message && not (Option.fold ~none:false ~some:(List.mem message) taken)
           | Leaves | Writes | Chooses _ -> false)
         program.cases
  in
  let inert message =
    match taking message with
    | Some (_, Leaves) | Some (_, Chooses (true, true)) -> true
    | Some (_, (Writes | Commands _ | Chooses _)) | None -> false
  in
  let first_handler = program.messages + 7 + List.length program.cases in
  List.sort compare
    (List.filter_map
       (fun message -> match sent message with true -> None | false -> Some ("dead_message", 2 + message))
       (List.init program.messages Fun.id)
    @ List.filter_map Fun.id
        (List.mapi
           (fun index sends ->
             match List.for_all inert sends with true -> Some ("dead_handler", first_handler + index) | false -> None)
           program.clicks))

let dead_alerted program =
  let items = Ppxlib.Parse.implementation (Lexing.from_string (dead_source program)) in
  let found =
    object
      inherit [(string * int) list] Ppxlib.Ast_traverse.fold as super

      method! expression expression alerts =
        match expression.pexp_desc with
        | Pexp_letmodule
            ( { txt = Some "Melhole_dead"; _ },
              { pmod_desc = Pmod_constraint (_, { pmty_desc = Pmty_signature [ { psig_desc = Psig_value here; _ } ]; _ }); _ },
              reference ) ->
            List.fold_left
              (fun alerts (attribute : Ppxlib.attribute) ->
                match attribute.attr_payload with
                | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident kind; _ }; _ }, _); _ }, _); _ } ] ->
                    (kind, reference.pexp_loc.loc_start.pos_lnum) :: alerts
                | _ -> alerts)
              alerts here.pval_attributes
        | _ -> super#expression expression alerts
    end
  in
  List.sort compare (found#structure (Meltea_ppx.Dead.annotate items) [])

let dead_laws =
  [
    Test.make ~name:"Dead code: alerts stand at exactly the messages nothing sends and the handlers whose messages do nothing"
      ~count:1000 ~print:dead_source dead_program (fun program -> dead_alerted program = dead_expected program);
  ]

let () =
  exit (QCheck_base_runner.run_tests ~verbose:true (reconcile_laws ("random pairs", reconcile_input) @ reconcile_laws ("edits", edited_input) @ markup_laws @ hook_name_laws @ dom_props_laws @ fanout_laws @ shell_laws @ lattice_laws @ dead_laws))
