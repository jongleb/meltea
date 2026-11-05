open Testing

external test_async : string -> (unit -> unit Js.Promise.t) -> unit = "test"

external keyboard : string -> < key : string ; bubbles : bool > Js.t -> Dom.event = "KeyboardEvent"
[@@mel.new]

external bubbling : string -> < bubbles : bool > Js.t -> Dom.event = "Event" [@@mel.new]
external set_value : Dom.element -> string -> unit = "value" [@@mel.set]
external value_of : Dom.element -> string = "value" [@@mel.get]
external checked : Dom.element -> bool = "checked" [@@mel.get]
external class_name : Dom.element -> string = "className" [@@mel.get]
external select_all : Dom.element -> string -> Dom.nodeList = "querySelectorAll" [@@mel.send]
external elements : Dom.nodeList -> Dom.element array = "from" [@@mel.scope "Array"]

external find : Dom.element -> string -> Dom.element option = "querySelector"
[@@mel.send] [@@mel.return nullable]

external remove : Dom.element -> unit = "remove" [@@mel.send]
external active_element : Dom.document -> Dom.element = "activeElement" [@@mel.get]
external set_inner_html : Dom.element -> string -> unit = "innerHTML" [@@mel.set]
external document_prototype : prototype = "prototype" [@@mel.scope "Document"]

let go_to : string -> unit =
  [%mel.raw
    {|function (hash) { history.replaceState(null, "", hash); window.dispatchEvent(new Event("hashchange")); }|}]

let later : int -> unit Js.Promise.t =
  [%mel.raw {|function (delay) { return new Promise((resolve) => setTimeout(resolve, delay)); }|}]

let ( let* ) promise next = Js.Promise.then_ next promise

(** How an edit ends. *)
type ending = Press_enter | Press_escape | Leave

(** What a user does on the page; rows are counted among the shown ones. *)
type action =
  | Enter_new of string
  | Toggle_row of int
  | Destroy_row of int
  | Edit_row of int * string * ending
  | Toggle_everything
  | Clear
  | Go of string

type entry = { title : string; status : Todomvc.status }

(** What the page must show: the todos in order, the route and the new todo's field. *)
type page = { entries : entry list; filter : Todomvc.filter; field : string }

let shown page =
  List.filter_map
    (fun (index, entry) ->
      match (page.filter, entry.status) with
      | All, _ | Only Active, Active | Only Completed, Completed -> Some index
      | Only Active, Completed | Only Completed, Active -> None)
    (List.mapi (fun index entry -> (index, entry)) page.entries)

let expect_page page action =
  let change row revise =
    Option.fold ~none:page
      ~some:(fun chosen ->
        {
          page with
          entries =
            List.concat
              (List.mapi (fun index entry -> if index = chosen then revise entry else [ entry ]) page.entries);
        })
      (List.nth_opt (shown page) row)
  in
  match action with
  | Enter_new text -> (
      match String.trim text with
      | "" -> { page with field = text }
      | title -> { page with entries = page.entries @ [ { title; status = Active } ]; field = "" })
  | Toggle_row row ->
      change row (fun entry ->
          [ { entry with status = (match entry.status with Active -> Completed | Completed -> Active) } ])
  | Destroy_row row -> change row (fun _ -> [])
  | Edit_row (row, text, (Press_enter | Leave)) ->
      change row (fun entry ->
          match String.trim text with "" -> [] | title -> [ { entry with title } ])
  | Edit_row (_, _, Press_escape) -> page
  | Toggle_everything ->
      let status =
        match List.exists (fun entry -> entry.status = Active) page.entries with
        | true -> Todomvc.Completed
        | false -> Active
      in
      { page with entries = List.map (fun entry -> { entry with status }) page.entries }
  | Clear -> { page with entries = List.filter (fun entry -> entry.status = Active) page.entries }
  | Go hash ->
      {
        page with
        filter =
          (match hash with
          | "#/active" -> Only Active
          | "#/completed" -> Only Completed
          | _ -> All);
      }

let words = [| "milk"; "  tea "; ""; "   "; "a <b> & \"c\""; "bread" |]
let hashes = [| "#/"; "#/active"; "#/completed"; "#/nowhere" |]

let actions tape =
  let rec go page remaining collected =
    match remaining with
    | 0 -> List.rev collected
    | _ ->
        let rows = List.length (shown page) in
        let word () = words.(draw tape (Array.length words)) in
        let action =
          match (draw tape 9, rows) with
          | (0 | 1), _ | _, 0 -> Enter_new (word ())
          | 2, _ -> Toggle_row (draw tape rows)
          | 3, _ -> Destroy_row (draw tape rows)
          | 4, _ ->
              let row = draw tape rows in
              Edit_row (row, word (), [| Press_enter; Press_escape; Leave |].(draw tape 3))
          | 5, _ -> Toggle_everything
          | 6, _ -> Clear
          | _, _ -> Go hashes.(draw tape (Array.length hashes))
        in
        go (expect_page page action) (remaining - 1) (action :: collected)
  in
  go { entries = []; filter = All; field = "" } (4 + draw tape 16) []

let perform root action =
  let row index = (elements (select_all root ".todo-list li")).(index) in
  let type_in field text =
    set_value field text;
    dispatch field (bubbling "input" [%mel.obj { bubbles = true }])
  in
  let press field key = dispatch field (keyboard "keydown" [%mel.obj { key; bubbles = true }]) in
  match action with
  | Enter_new text ->
      let field = select root ".new-todo" in
      type_in field text;
      press field "Enter";
      later 0
  | Toggle_row index ->
      click (select (row index) ".toggle");
      later 0
  | Destroy_row index ->
      click (select (row index) ".destroy");
      later 0
  | Edit_row (index, text, ending) ->
      dispatch (select (row index) "label") (bubbling "dblclick" [%mel.obj { bubbles = true }]);
      let* () = later 0 in
      let field = select root ".edit" in
      type_in field text;
      let* () = later 0 in
      (match ending with
      | Press_enter -> press field "Enter"
      | Press_escape -> press field "Escape"
      | Leave -> dispatch field (bubbling "focusout" [%mel.obj { bubbles = true }]));
      later 0
  | Toggle_everything ->
      Option.iter click (find root ".toggle-all");
      later 0
  | Clear ->
      Option.iter click (find root ".clear-completed");
      later 0
  | Go hash ->
      go_to hash;
      later 0

let observe root =
  ( Array.to_list
      (Array.map
         (fun row -> (text_content (select row "label"), class_name row))
         (elements (select_all root ".todo-list li"))),
    Option.map text_content (find root ".todo-count"),
    Option.map checked (find root ".toggle-all"),
    Option.map text_content (find root ".clear-completed"),
    Option.map text_content (find root ".filters .selected"),
    value_of (select root ".new-todo"),
    Array.length (elements (select_all root ".edit")) )

let picture page =
  let left = List.length (List.filter (fun entry -> entry.status = Todomvc.Active) page.entries) in
  let present value = match page.entries with [] -> None | _ :: _ -> Some value in
  ( List.map
      (fun index ->
        let entry = List.nth page.entries index in
        (entry.title, match entry.status with Active -> "" | Completed -> "completed"))
      (shown page),
    present (string_of_int left ^ if left = 1 then " item left" else " items left"),
    present (left = 0),
    (match left < List.length page.entries with true -> Some "Clear completed" | false -> None),
    present (match page.filter with All -> "All" | Only Active -> "Active" | Only Completed -> "Completed"),
    page.field,
    0 )

let start () =
  go_to "#/";
  let root = attached () in
  let program =
    Program.element
      ~init:(Todomvc.init ~saved:"[]" ~hash:"#/")
      ~update:Todomvc.update ~view:Todomvc.view ~subscriptions:Todomvc.subscriptions root
  in
  (root, program)

let finish root program =
  Program.stop program;
  remove root

let () =
  describe "TodoMVC" (fun () ->
    test_async "after any user actions the page shows the todos, the count and the route" (fun () ->
      check_async
        (async_property (tapes ~shortest:10 ~longest:60) (fun values ->
             let actions = actions (tape values) in
             let root, program = start () in
             let rec run page remaining seen =
               match remaining with
               | [] ->
                   finish root program;
                   Js.Promise.resolve (List.rev seen)
               | action :: rest ->
                   let page = expect_page page action in
                   let* () = perform root action in
                   run page rest ((observe root, picture page) :: seen)
             in
             let* seen = run { entries = []; filter = All; field = "" } actions [] in
             expect (List.map fst seen) |. to_equal (List.map snd seen);
             Js.Promise.resolve ()))
        [%mel.obj { numRuns = 80 }]);
    test_async "a double-click focuses that row's edit field by the next frame" (fun () ->
      check_async
        (async_property (tapes ~shortest:2 ~longest:4) (fun values ->
             let tape = tape values in
             let count = 1 + draw tape 4 in
             let chosen = draw tape count in
             let root, program = start () in
             let rec add remaining =
               match remaining with
               | 0 -> Js.Promise.resolve ()
               | _ ->
                   let* () = perform root (Enter_new "milk") in
                   add (remaining - 1)
             in
             let* () = add count in
             let label = select (elements (select_all root ".todo-list li")).(chosen) "label" in
             dispatch label (bubbling "dblclick" [%mel.obj { bubbles = true }]);
             let* () = later 40 in
             let focused = same (active_element document) (select root ".edit") in
             finish root program;
             expect focused |. to_be true;
             Js.Promise.resolve ()))
        [%mel.obj { numRuns = 8 }]);
    test "a message the compiler marks as changing nothing keeps the model, and Ignore is one" (fun () ->
      let fields =
        match Todomvc.__melhole_changes_update with
        | Program.Changes { fields; _ } -> fields
        | Program.Unanalysed -> fun _ -> -1
      in
      check
        (property (tapes ~shortest:2 ~longest:30) (fun values ->
             let tape = tape values in
             let model, _ = Todomvc.init ~saved:(Todomvc.encode [ { Todomvc.id = 0; title = "a"; status = Active } ]) ~hash:"#/" in
             let model =
               List.fold_left
                 (fun model message -> fst (Todomvc.update message model))
                 model
                 (List.init (draw tape 4) (fun _ -> Todomvc.Type words.(draw tape (Array.length words))))
             in
             let messages =
               Todomvc.
                 [ Type "x"; Add; Toggle 0; Toggle_all; Destroy 0; Clear_completed; Edit 0; Retype "y"; Commit; Cancel; Route All; Ignore ]
             in
             let message = List.nth messages (draw tape (List.length messages)) in
             let next, _ = Todomvc.update message model in
             expect
               ( (match fields message with 0 -> same next model | _ -> true),
                 fields Todomvc.Ignore )
             |. to_equal (true, 0)))
        [%mel.obj { numRuns = 300 }]);
    test_async "hydrating the server's page shows the same page, creates no element, and keeps working" (fun () ->
      check_async
        (async_property (tapes ~shortest:10 ~longest:60) (fun values ->
             let tape = tape values in
             go_to "#/";
             let model =
               List.fold_left
                 (fun model _ ->
                   let message =
                     match draw tape 7 with
                     | 0 | 1 -> Todomvc.Type words.(draw tape (Array.length words))
                     | 2 -> Todomvc.Add
                     | 3 -> Todomvc.Toggle (draw tape 4)
                     | 4 -> Todomvc.Toggle_all
                     | 5 -> Todomvc.Edit (draw tape 4)
                     | _ -> Todomvc.Route [| Todomvc.All; Only Active; Only Completed |].(draw tape 3)
                   in
                   fst (Todomvc.update message model))
                 (fst (Todomvc.init ~saved:"[]" ~hash:"#/"))
                 (List.init (draw tape 14) Fun.id)
             in
             let served = attached () and fresh = attached () in
             set_inner_html served (Program.render ~view:Todomvc.view model);
             let creates = spy_on document_prototype "createElement" in
             let clones = spy_on node_prototype "cloneNode" in
             let hydrated =
               Program.element ~start:Program.hydrate ~init:(model, Cmd.none) ~update:Todomvc.update ~view:Todomvc.view
                 ~subscriptions:Todomvc.subscriptions served
             in
             let created = Array.length (calls (mock creates)) + Array.length (calls (mock clones)) in
             restore creates;
             restore clones;
             let mounted =
               Program.element ~init:(model, Cmd.none) ~update:Todomvc.update ~view:Todomvc.view
                 ~subscriptions:Todomvc.subscriptions fresh
             in
             let first = (observe served, shapes (as_node served)) = (observe fresh, shapes (as_node fresh)) in
             let actions =
               List.init (1 + draw tape 5) (fun _ ->
                   match draw tape 4 with
                   | 0 -> Enter_new words.(draw tape (Array.length words))
                   | 1 -> Toggle_everything
                   | 2 -> Clear
                   | _ -> Go hashes.(draw tape (Array.length hashes)))
             in
             let rec run remaining agreed =
               match remaining with
               | [] -> Js.Promise.resolve agreed
               | action :: rest ->
                   let* () = perform served action in
                   let* () = perform fresh action in
                   run rest (agreed && observe served = observe fresh)
             in
             let* agreed = run actions true in
             finish served hydrated;
             finish fresh mounted;
             expect (created, first, agreed) |. to_equal (0, true, true);
             Js.Promise.resolve ()))
        [%mel.obj { numRuns = 60 }]);
    test "decode inverts encode" (fun () ->
      check
        (property (tapes ~shortest:0 ~longest:30) (fun values ->
             let tape = tape values in
             let todos =
               List.init (draw tape 8) (fun index ->
                   {
                     Todomvc.id = 100 + draw tape 100;
                     title = words.(draw tape (Array.length words)) ^ string_of_int index;
                     status = [| Todomvc.Active; Completed |].(draw tape 2);
                   })
             in
             expect (Todomvc.decode (Todomvc.encode todos)) |. to_equal todos))
        [%mel.obj { numRuns = 300 }]);
    test "decode reads any saved text without throwing, and what it reads saves back the same" (fun () ->
      let pieces =
        [| "["; "]"; "{"; "}"; ","; ":"; "\"title\""; "\"completed\""; "\"x\""; "true"; "null"; "1" |]
      in
      check
        (property (tapes ~shortest:0 ~longest:40) (fun values ->
             let tape = tape values in
             let text =
               String.concat ""
                 (List.init (Array.length values) (fun _ -> pieces.(draw tape (Array.length pieces))))
             in
             let decoded = Todomvc.decode text in
             expect (Todomvc.decode (Todomvc.encode decoded)) |. to_equal decoded))
        [%mel.obj { numRuns = 500 }]))
