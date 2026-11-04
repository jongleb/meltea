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
            match step with Keep i | Move i -> [ (j, i) ] | Create -> [])
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
         (fun i -> if List.mem i plan.dropped then None else Some (`Old i))
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
      let node = match plan.steps.(j) with Create -> `New j | Keep i | Move i -> `Old i in
      (match plan.steps.(j) with Create | Move _ -> place node anchor | Keep _ -> ());
      walk (j - 1) (Some node)
    end
  in
  walk (Array.length next - 1) None;
  !dom

let reconcile_laws =
  [
    Test.make ~count:2000 ~name:"Reconcile: one step per new entry" ~print:print_keys reconcile_input
      (fun (old, next) -> Array.length (Reconcile.plan ~old ~next).steps = Array.length next);
    Test.make ~count:2000 ~name:"Reconcile: reused and dropped entries partition the old ones"
      ~print:print_keys reconcile_input (fun (old, next) ->
        let plan = Reconcile.plan ~old ~next in
        List.sort compare (List.map snd (reused plan) @ plan.dropped)
        = List.init (Array.length old) Fun.id);
    Test.make ~count:2000 ~name:"Reconcile: a reused entry has the same key" ~print:print_keys
      reconcile_input (fun (old, next) ->
        List.for_all
          (fun (j, i) -> String.equal old.(i) next.(j))
          (reused (Reconcile.plan ~old ~next)));
    Test.make ~count:2000 ~name:"Reconcile: reuse is maximal, the sum over keys of min(old, new) counts"
      ~print:print_keys reconcile_input (fun (old, next) ->
        let keys = List.sort_uniq compare (Array.to_list old @ Array.to_list next) in
        List.length (reused (Reconcile.plan ~old ~next))
        = List.fold_left (fun total key -> total + min (count key old) (count key next)) 0 keys);
    Test.make ~count:2000 ~name:"Reconcile: kept entries appear in increasing old order" ~print:print_keys
      reconcile_input (fun (old, next) ->
        let kept =
          List.concat_map
            (fun (step : Reconcile.step) -> match step with Keep i -> [ i ] | Move _ | Create -> [])
            (Array.to_list (Reconcile.plan ~old ~next).steps)
        in
        kept = List.sort_uniq compare kept);
    Test.make ~count:2000 ~name:"Reconcile: kept entries are a longest increasing subsequence (moves are minimal)"
      ~print:print_keys reconcile_input (fun (old, next) ->
        let plan = Reconcile.plan ~old ~next in
        let kept =
          Array.fold_left
            (fun total (step : Reconcile.step) ->
              match step with Keep _ -> total + 1 | Move _ | Create -> total)
            0 plan.steps
        in
        kept = longest_increasing (List.map snd (reused plan)));
    Test.make ~count:2000 ~name:"Reconcile: executing the plan right to left yields exactly the new sequence"
      ~print:print_keys reconcile_input (fun (old, next) ->
        let plan = Reconcile.plan ~old ~next in
        simulate old next plan
        = Array.to_list
            (Array.mapi
               (fun j (step : Reconcile.step) ->
                 match step with Create -> `New j | Keep i | Move i -> `Old i)
               plan.steps));
    Test.make ~count:2000 ~name:"Reconcile: an unchanged sequence keeps every entry in place"
      ~print:Print.(array string) reconcile_keys (fun keys ->
        let plan = Reconcile.plan ~old:keys ~next:keys in
        plan.dropped = []
        && Array.for_all2
             (fun (step : Reconcile.step) i -> step = Keep i)
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
      | On_element _ -> [])
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
            | Attribute (name, _) | Property (name, _) ->
                well_formed name
                && (String.length label < 5
                   || (not (String.sub label 0 4 = "aria" && Char.uppercase_ascii label.[4] = label.[4]))
                   || String.equal name ("aria-" ^ String.lowercase_ascii (String.sub label 4 (String.length label - 4))))
            | Handler (event, family) ->
                String.for_all (fun character -> character >= 'a' && character <= 'z') event
                && String.length family > 0
                && Char.uppercase_ascii family.[0] = family.[0]
            | Style | Ref | Inner_html | Ignored -> true)
          all);
  ]

let () =
  exit (QCheck_base_runner.run_tests ~verbose:true (reconcile_laws @ markup_laws @ dom_props_laws))
