(** A value of any type, compared physically. *)
type input = Input : 'a -> input [@@unboxed]

(** Whether a value differs from the one remembered for it. *)
type change = Changed | Unchanged

(** Whether a template's root is in the document yet. *)
type placement = Detached | Placed

(** A mounted template: the values its refresh last read, what to release, which refs to call. *)
type scope = {
  deps : input array;
  mutable cleanups : (unit -> unit[@u]) list;  (** Newest first. *)
  mutable references : (unit -> unit[@u]) list;  (** Called once the root is placed. *)
  mutable placement : placement;
}

(** What a library that mounts its own kind of child keeps in it. *)
type hosted = ..

(** A mounted row of a compiled loop: its root, scope and holes, and the witness of their type. *)
type mounted_row =
  | Mounted_row : { root : Dom.element; scope : scope; holes : 'holes; witness : 'holes Type.Id.t } -> mounted_row

(** What a mounted child is, so that the next child can tell whether it can patch it. *)
type state =
  | Empty_state
  | Text_state of { node : Dom.text; mutable value : string }
  | List_state of live array
  | Keyed_state of string array * live array
  | Memo_state of input array * live
  | Pure_state of { identity : input; inputs : input array; inner : live }
      (** A function of its inputs; another identity is another kind of child. *)
  | Portal_state of Dom.element * live
  | Compiled_state of { identity : input; scope : scope; lend : (unit -> unit[@u]) }
  | Static_state of Template.t
  | Paths_state of Template.t * (hole array -> unit)  (** Writes the next values to its holes. *)
  | Hosted of hosted
  | Each_state : {
      items : 'item array;
      keys : string array;
      outer : input array;  (** What the rendering reads besides the item. *)
      projections : input array array;  (** Per row, the parts of its rendering that read [outer]. *)
      lives : live array;
    }
      -> state
  | Rows_state : {
      items : 'item array;
      keys : string array;
      outer : input array;
      projections : input array array;
      rows : mounted_row array;
    }
      -> state

(** A mounted child: its own nodes and how to let go of what they hold. *)
and live = {
  state : state;
  each : ((Dom.node -> unit[@u]) -> unit[@u]);  (** Visits the top-level nodes in document order. *)
  front : (Dom.node Js.null -> Dom.node Js.null[@u]);  (** The first node, or the given one. *)
  release : (unit -> unit[@u]);  (** Releases handlers, effects and portals; the nodes stay. *)
}

(** How a child takes part in [list] and [items]. *)
and shape =
  | Single
  | Nothing
  | Entry of string * child  (** A keyed child. *)
  | Many of child list
  | Entries of (string * child) list
  | Deferred of (unit -> child)  (** Rendered on demand. *)

(** A description that mounts itself and patches a live child of its own kind. *)
and child = {
  build : (Template.position -> live[@u]);
  patch : (Template.position -> live -> live option[@u]);  (** [None] for another kind of child. *)
  shape : shape;
}

and hole =
  | HText of string
  | HAttr of string option
  | HProp : 'a -> hole
  | HClass of string list
  | HStyle of (string * string) list
  | HHandler of (Dom.event -> unit)
  | HRef of (Dom.element -> unit)
  | HChild of child

(** A compiled template: its markup and how to make its holes on a fresh clone. *)
type 'holes template = {
  markup : Template.t;
  holes : (scope -> Dom.node -> 'holes[@u]);
  mutable lent : 'holes option;  (** The holes of a mounted clone, handed over to one refresh. *)
  witness : 'holes Type.Id.t;  (** Proves that holes made by this template have its type. *)
}

(** One row of a compiled loop before it is mounted: its template, how to write its holes, its key. *)
type 'holes row = { template : 'holes template; slots : int; refresh : (scope -> 'holes -> unit[@u]); key : string }

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

let nothing =
  { state = Empty_state; each = (fun [@u] _ -> ()); front = (fun [@u] after -> after); release = (fun [@u] () -> ()) }

let insert (position : Template.position) node =
  Dom_api.insert_before position.parent node position.before

(** Whether a content hole still has the server's markers around its place. *)
type range = Settled | Closing_at of Dom.node

(** How children reach the DOM: built in the browser, written by a server, or adopted from its HTML. *)
type medium = {
  text : (Template.position -> string option -> Dom.text[@u]);  (** [None] keeps a text already there. *)
  root : (Template.position -> Template.t -> Dom.element[@u]);
  placed : (Template.position -> Dom.element -> unit[@u]);
  opened : (Template.position -> range[@u]);  (** Where a content hole's children go. *)
  closed : (Dom.node -> Dom.node -> range[@u]);  (** After a hole's first children, before its closing marker. *)
  built :
    'made.
    (int ->
    Template.position ->
    (int -> Template.position -> 'made[@u]) ->
    ('made -> Dom.node Js.null -> Dom.node Js.null[@u]) ->
    'made array[@u]);
  aside : ((unit -> live[@u]) -> live[@u]);  (** Children that live elsewhere, like a portal's. *)
}

let browser =
  {
    text =
      (fun [@u] position value ->
        let node = Dom_api.create_text_node Dom_api.document (Option.value ~default:"" value) in
        insert position node;
        node);
    root = (fun [@u] _ markup -> Template.clone markup);
    placed = (fun [@u] position root -> insert position root);
    opened = (fun [@u] _ -> Settled);
    closed = (fun [@u] _ _ -> Settled);
    built = (fun [@u] count position make _ -> Array.init count (fun index -> make index position [@u]));
    aside = (fun [@u] build -> build () [@u]);
  }

let medium = ref browser

let remove (position : Template.position) live =
  live.each (fun [@u] node -> Dom_api.remove_child position.parent node) [@u];
  live.release () [@u]

(** Whether a live child's nodes are all the children of its parent. *)
type extent = Whole | Part

let extent (position : Template.position) live =
  let last = ref Js.null in
  live.each (fun [@u] node -> last := Js.Null.return node) [@u];
  match
    ( Dom_api.same (live.front Js.null [@u]) (Template.first_or_end position.parent),
      Dom_api.same !last (Dom_api.last_child position.parent) )
  with
  | true, true -> Whole
  | true, false | false, _ -> Part

let update position live child =
  match child.patch position live [@u] with
  | Some next -> next
  | None -> (
      match extent position live with
      | Whole ->
          Dom_api.set_text_content position.parent "";
          live.release () [@u];
          child.build position [@u]
      | Part ->
          let next = child.build position [@u] in
          remove position live;
          next)

let compare_inputs old next =
  match
    Array.length old = Array.length next
    && Array.for_all2 (fun (Input a) (Input b) -> Dom_api.same a b) old next
  with
  | true -> Unchanged
  | false -> Changed

let group state items =
  {
    state;
    each = (fun [@u] visit -> Array.iter (fun item -> item.each visit [@u]) items);
    front =
      (fun [@u] after ->
        let rec first index =
          match index = Array.length items with
          | true -> after
          | false ->
              let found = (Array.unsafe_get items index).front Js.null [@u] in
              if Dom_api.same found Js.null then first (index + 1) else found
        in
        first 0);
    release = (fun [@u] () -> Array.iter (fun item -> item.release () [@u]) items);
  }

let backwards count (position : Template.position) make front =
  match count with
  | 0 -> [||]
  | _ ->
      let last = make (count - 1) position [@u] in
      let result = Array.make count last in
      let rec fill i before =
        if i >= 0 then (
          let made = make i { position with before } [@u] in
          Array.unsafe_set result i made;
          fill (i - 1) (front made before [@u]))
      in
      fill (count - 2) (front last position.before [@u]);
      result

let front_of = fun [@u] (live : live) after -> live.front after [@u]

let text value =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let node = !medium.text position (Some value) [@u] in
        {
          state = Text_state { node; value };
          each = (fun [@u] visit -> visit (Dom_api.to_node node) [@u]);
          front = (fun [@u] _ -> Js.Null.return (Dom_api.to_node node));
          release = (fun [@u] () -> ());
        });
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Text_state site ->
            if not (Dom_api.same site.value value) then (
              Dom_api.set_data site.node value;
              site.value <- value);
            Some live
        | _ -> None);
  }

let empty =
  {
    shape = Nothing;
    build = (fun [@u] _ -> nothing);
    patch = (fun [@u] _ live -> match live.state with Empty_state -> Some live | _ -> None);
  }

let many children =
  {
    shape = Many children;
    build =
      (fun [@u] position ->
        let children = Array.of_list children in
        let items = !medium.built (Array.length children) position (fun [@u] index at -> (Array.unsafe_get children index).build at [@u]) front_of [@u] in
        group (List_state items) items);
    patch =
      (fun [@u] position live ->
        match live.state with
        | List_state items ->
            let children = Array.of_list children in
            let old_count = Array.length items in
            for i = Array.length children to old_count - 1 do
              remove position (Array.unsafe_get items i)
            done;
            let items =
              backwards (Array.length children) position (fun [@u] i at ->
                  if i < old_count then update at (Array.unsafe_get items i) (Array.unsafe_get children i)
                  else (Array.unsafe_get children i).build at [@u])
                front_of
            in
            Some (group (List_state items) items)
        | _ -> None);
  }

let keyed entries =
  let keys = Array.of_list (List.map fst entries) in
  let children = Array.of_list (List.map snd entries) in
  {
    shape = Entries entries;
    build =
      (fun [@u] position ->
        let items = !medium.built (Array.length children) position (fun [@u] index at -> (Array.unsafe_get children index).build at [@u]) front_of [@u] in
        group (Keyed_state (keys, items)) items);
    patch =
      (fun [@u] position live ->
        match live.state with
        | Keyed_state (old_keys, items) ->
            let plan = Reconcile.plan ~old:old_keys ~next:keys in
            (match (List.compare_length_with plan.dropped (Array.length items), extent position live) with
            | 0, Whole ->
                Dom_api.set_text_content position.parent "";
                live.release () [@u]
            | _, (Whole | Part) -> List.iter (fun i -> remove position (Array.unsafe_get items i)) plan.dropped);
            let items =
              backwards (Array.length children) position (fun [@u] j at ->
                  match Array.unsafe_get plan.steps j with
                  | Create -> (Array.unsafe_get children j).build at [@u]
                  | Keep i -> update at (Array.unsafe_get items i) (Array.unsafe_get children j)
                  | Move i ->
                      let live = update at (Array.unsafe_get items i) (Array.unsafe_get children j) in
                      live.each (fun [@u] node -> insert at node) [@u];
                      live)
                front_of
            in
            Some (group (Keyed_state (keys, items)) items)
        | _ -> None);
  }

let with_key key child = { child with shape = Entry (key, child) }

(** A row of the next render of a loop: the old row kept as it is, or what to patch it with. *)
type 'fresh next = Kept of int | Fresh of 'fresh

let each items render project outer =
  let count = Array.length items in
  let rendered index item =
    let child = render item [@u] in
    match child.shape with
    | Entry (key, inner) -> (inner, key)
    | Single | Nothing | Many _ | Entries _ | Deferred _ -> (child, string_of_int index)
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let keys = Array.make count "" in
        let lives =
          !medium.built count position
            (fun [@u] index at ->
              let child, key = rendered index (Array.unsafe_get items index) in
              Array.unsafe_set keys index key;
              child.build at [@u])
            front_of [@u]
        in
        let projections = Array.map (fun item -> project item [@u]) items in
        group (Each_state { items; keys; outer; projections; lives }) lives);
    patch =
      (fun [@u] position live ->
        match live.state with
        | Each_state old ->
            let old_count = Array.length old.items in
            let rows = Array.make count (Kept 0) and keys = Array.make count "" in
            let projections = Array.make count [||] in
            let outer_change = compare_inputs old.outer outer in
            let shift = ref 0 in
            Array.iteri
              (fun index item ->
                let projected =
                  match outer_change with
                  | Unchanged -> None
                  | Changed -> Some (project item [@u])
                in
                let fits from =
                  from >= 0 && from < old_count
                  && Dom_api.same (Array.unsafe_get old.items from) item
                  && Option.fold ~none:true
                       ~some:(fun projection ->
                         match compare_inputs (Array.unsafe_get old.projections from) projection with
                         | Unchanged -> true
                         | Changed -> false)
                       projected
                in
                let reuse from =
                  Array.unsafe_set rows index (Kept from);
                  Array.unsafe_set keys index (Array.unsafe_get old.keys from);
                  Array.unsafe_set projections index
                    (Option.value ~default:(Array.unsafe_get old.projections from) projected)
                in
                if fits (index + !shift) then reuse (index + !shift)
                else if fits (index + !shift + 1) then (
                  incr shift;
                  reuse (index + !shift))
                else if fits (index + !shift - 1) then (
                  decr shift;
                  reuse (index + !shift))
                else (
                  let child, key = rendered index item in
                  Array.unsafe_set rows index (Fresh child);
                  Array.unsafe_set keys index key;
                  Array.unsafe_set projections index
                    (match projected with Some projection -> projection | None -> project item [@u])))
              items;
            let described index =
              match Array.unsafe_get rows index with
              | Fresh child -> child
              | Kept _ -> fst (rendered index (Array.unsafe_get items index))
            in
            let rec order index =
              match index = count with
              | true -> `Same
              | false ->
                  if String.equal (Array.unsafe_get keys index) (Array.unsafe_get old.keys index) then
                    order (index + 1)
                  else `Moved
            in
            (match (count = old_count, order 0) with
            | true, `Same ->
                let lives = Array.copy old.lives in
                let rec ahead index =
                  match index = count with
                  | true -> position.before
                  | false ->
                      let found = (Array.unsafe_get lives index).front Js.null [@u] in
                      if Dom_api.same found Js.null then ahead (index + 1) else found
                in
                let rec refresh index =
                  if index >= 0 then (
                    (match Array.unsafe_get rows index with
                    | Kept from when from = index -> ()
                    | Kept _ | Fresh _ ->
                        Array.unsafe_set lives index
                          (update
                             { position with before = ahead (index + 1) }
                             (Array.unsafe_get old.lives index)
                             (described index)));
                    refresh (index - 1))
                in
                refresh (count - 1);
                Some (group (Each_state { items; keys; outer; projections; lives }) lives)
            | _, (`Same | `Moved) ->
                let plan = Reconcile.plan ~old:old.keys ~next:keys in
                (match (List.compare_length_with plan.dropped old_count, extent position live) with
                | 0, Whole ->
                    Dom_api.set_text_content position.parent "";
                    live.release () [@u]
                | _, (Whole | Part) ->
                    List.iter (fun i -> remove position (Array.unsafe_get old.lives i)) plan.dropped);
                let carried at i j =
                  match Array.unsafe_get rows j with
                  | Kept from when from = i -> Array.unsafe_get old.lives i
                  | Kept _ | Fresh _ -> update at (Array.unsafe_get old.lives i) (described j)
                in
                let lives =
                  backwards count position (fun [@u] j at ->
                      match Array.unsafe_get plan.steps j with
                      | Create -> (described j).build at [@u]
                      | Keep i -> carried at i j
                      | Move i ->
                          let live = carried at i j in
                          live.each (fun [@u] node -> insert at node) [@u];
                          live)
                    front_of
                in
                Some (group (Each_state { items; keys; outer; projections; lives }) lives))
        | _ -> None);
  }

let list children =
  let rec entries collected children =
    match children with
    | [] -> Some (List.rev collected)
    | { shape = Entry (key, child); _ } :: rest -> entries ((key, child) :: collected) rest
    | { shape = Single | Nothing | Many _ | Entries _ | Deferred _; _ } :: _ -> None
  in
  match children with
  | [] -> many []
  | _ :: _ -> Option.fold ~none:(many children) ~some:keyed (entries [] children)

let rec items child =
  match child.shape with
  | Nothing -> []
  | Many children -> List.concat_map items children
  | Entries entries -> List.map (fun (key, child) -> with_key key child) entries
  | Deferred render -> items (render ())
  | Single | Entry _ -> [ child ]

let memo inputs render =
  let skipping inner = { inner with state = Memo_state (inputs, inner) } in
  {
    shape = Deferred render;
    build = (fun [@u] position -> skipping ((render ()).build position [@u]));
    patch =
      (fun [@u] position live ->
        match live.state with
        | Memo_state (old, inner) -> (
            match compare_inputs old inputs with
            | Unchanged -> Some live
            | Changed -> Some (skipping (update position inner (render ()))))
        | _ -> None);
  }

let pure identity inputs render =
  let named inner = { inner with state = Pure_state { identity = Input identity; inputs; inner } } in
  {
    shape = Single;
    build = (fun [@u] position -> named ((render ()).build position [@u]));
    patch =
      (fun [@u] position live ->
        match live.state with
        | Pure_state old -> (
            let (Input known) = old.identity in
            match (Dom_api.same known identity, compare_inputs old.inputs inputs) with
            | true, Unchanged -> Some live
            | true, Changed -> Some (named (update position old.inner (render ())))
            | false, _ -> None)
        | _ -> None);
  }

let lazy1 render a = memo [| Input render; Input a |] (fun () -> render a)
let lazy2 render a b = memo [| Input render; Input a; Input b |] (fun () -> render a b)

let portal container child =
  let inside = { Template.parent = Dom_api.to_node container; before = Js.null } in
  let held inner =
    {
      state = Portal_state (container, inner);
      each = (fun [@u] _ -> ());
      front = (fun [@u] after -> after);
      release =
        (fun [@u] () ->
          inner.each (fun [@u] node -> Dom_api.remove_child (Dom_api.to_node container) node) [@u];
          inner.release () [@u]);
    }
  in
  {
    shape = Single;
    build =
      (fun [@u] _ -> held (!medium.aside (fun [@u] () -> child.build inside [@u]) [@u]));
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Portal_state (target, inner) when Dom_api.same target container ->
            Some (held (update inside inner child))
        | _ -> None);
  }

let settle position root scope =
  !medium.placed position root [@u];
  scope.placement <- Placed;
  List.iter (fun reference -> reference () [@u]) (List.rev scope.references)

let let_go scope = List.iter (fun cleanup -> cleanup () [@u]) (List.rev scope.cleanups)

let place position root scope state =
  settle position root scope;
  {
    state;
    each = (fun [@u] visit -> visit (Dom_api.to_node root) [@u]);
    front = (fun [@u] _ -> Js.Null.return (Dom_api.to_node root));
    release = (fun [@u] () -> let_go scope);
  }

let unset = Input (ref ())
let fresh count = { deps = Array.make count unset; cleanups = []; references = []; placement = Detached }

let template markup holes = { markup; holes; lent = None; witness = Type.Id.make () }

let row template slots refresh key = { template; slots; refresh; key }

let rows (type holes item) (items : item array) (render : (item -> holes row)[@u]) project outer =
  let count = Array.length items in
  let mount (position : Template.position) parts =
    let root = !medium.root position parts.template.markup [@u] in
    let scope = fresh parts.slots in
    let holes = parts.template.holes scope (Dom_api.to_node root) [@u] in
    parts.refresh scope holes [@u];
    settle position root scope;
    Mounted_row { root; scope; holes; witness = parts.template.witness }
  in
  let root_of (Mounted_row row) = Dom_api.to_node row.root in
  let front = fun [@u] row _ -> Js.Null.return (root_of row) in
  let shown items keys projections rows =
    {
      state = Rows_state { items; keys; outer; projections; rows };
      each = (fun [@u] visit -> Array.iter (fun row -> visit (root_of row) [@u]) rows);
      front =
        (fun [@u] after ->
          match Array.length rows with 0 -> after | _ -> Js.Null.return (root_of (Array.unsafe_get rows 0)));
      release = (fun [@u] () -> Array.iter (fun (Mounted_row row) -> let_go row.scope) rows);
    }
  in
  let drop (position : Template.position) (Mounted_row row) =
    Dom_api.remove_child position.parent row.root;
    let_go row.scope
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let keys = Array.make count "" in
        let rows =
          !medium.built count position
            (fun [@u] index at ->
              let parts = render (Array.unsafe_get items index) [@u] in
              Array.unsafe_set keys index parts.key;
              mount at parts)
            front [@u]
        in
        shown items keys (Array.map (fun item -> project item [@u]) items) rows);
    patch =
      (fun [@u] position live ->
        match live.state with
        | Rows_state old ->
            let old_count = Array.length old.items in
            let next = Array.make count (Kept 0) and keys = Array.make count "" in
            let projections = Array.make count [||] in
            let outer_change = compare_inputs old.outer outer in
            let shift = ref 0 in
            Array.iteri
              (fun index item ->
                let projected =
                  match outer_change with Unchanged -> None | Changed -> Some (project item [@u])
                in
                let fits from =
                  from >= 0 && from < old_count
                  && Dom_api.same (Array.unsafe_get old.items from) item
                  && Option.fold ~none:true
                       ~some:(fun projection ->
                         match compare_inputs (Array.unsafe_get old.projections from) projection with
                         | Unchanged -> true
                         | Changed -> false)
                       projected
                in
                let reuse from =
                  Array.unsafe_set next index (Kept from);
                  Array.unsafe_set keys index (Array.unsafe_get old.keys from);
                  Array.unsafe_set projections index
                    (Option.value ~default:(Array.unsafe_get old.projections from) projected)
                in
                if fits (index + !shift) then reuse (index + !shift)
                else if fits (index + !shift + 1) then (
                  incr shift;
                  reuse (index + !shift))
                else if fits (index + !shift - 1) then (
                  decr shift;
                  reuse (index + !shift))
                else (
                  let parts = render item [@u] in
                  Array.unsafe_set next index (Fresh parts);
                  Array.unsafe_set keys index parts.key;
                  Array.unsafe_set projections index
                    (match projected with Some projection -> projection | None -> project item [@u])))
              items;
            let drawn index =
              match Array.unsafe_get next index with
              | Fresh parts -> parts
              | Kept _ -> render (Array.unsafe_get items index) [@u]
            in
            let refreshed (Mounted_row row as mounted) (parts : holes row) =
              match Type.Id.provably_equal row.witness parts.template.witness with
              | Some Equal ->
                  parts.refresh row.scope row.holes [@u];
                  mounted
              | None ->
                  let made =
                    mount { position with before = Dom_api.following_sibling (Dom_api.to_node row.root) } parts
                  in
                  drop position mounted;
                  made
            in
            let rec order index =
              match index = count with
              | true -> `Same
              | false ->
                  if String.equal (Array.unsafe_get keys index) (Array.unsafe_get old.keys index) then
                    order (index + 1)
                  else `Moved
            in
            (match (count = old_count, order 0) with
            | true, `Same ->
                let rows =
                  Array.mapi
                    (fun index mounted ->
                      match Array.unsafe_get next index with
                      | Kept from when from = index -> mounted
                      | Kept _ | Fresh _ -> refreshed mounted (drawn index))
                    old.rows
                in
                Some (shown items keys projections rows)
            | _, (`Same | `Moved) ->
                let plan = Reconcile.plan ~old:old.keys ~next:keys in
                let whole =
                  match List.compare_length_with plan.dropped old_count with
                  | 0 when old_count > 0 -> (
                      match
                        ( Dom_api.same
                            (Js.Null.return (root_of (Array.unsafe_get old.rows 0)))
                            (Template.first_or_end position.parent),
                          Dom_api.same
                            (Js.Null.return (root_of (Array.unsafe_get old.rows (old_count - 1))))
                            (Dom_api.last_child position.parent) )
                      with
                      | true, true -> `Whole
                      | true, false | false, _ -> `Part)
                  | _ -> `Part
                in
                (match whole with
                | `Whole ->
                    Dom_api.set_text_content position.parent "";
                    Array.iter (fun (Mounted_row row) -> let_go row.scope) old.rows
                | `Part -> List.iter (fun i -> drop position (Array.unsafe_get old.rows i)) plan.dropped);
                let carried i j =
                  match Array.unsafe_get next j with
                  | Kept from when from = i -> Array.unsafe_get old.rows i
                  | Kept _ | Fresh _ -> refreshed (Array.unsafe_get old.rows i) (drawn j)
                in
                let rows =
                  backwards count position
                    (fun [@u] j at ->
                      match Array.unsafe_get plan.steps j with
                      | Create -> mount at (drawn j)
                      | Keep i -> carried i j
                      | Move i ->
                          let moved = carried i j in
                          insert at (root_of moved);
                          moved)
                    front
                in
                Some (shown items keys projections rows))
        | _ -> None);
  }


let compiled template count refresh =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root = !medium.root position template.markup [@u] in
        let scope = fresh count in
        let holes = template.holes scope (Dom_api.to_node root) [@u] in
        refresh scope holes [@u];
        place position root scope
          (Compiled_state
             { identity = Input template; scope; lend = (fun [@u] () -> template.lent <- Some holes) }));
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Compiled_state mounted ->
            let (Input identity) = mounted.identity in
            if Dom_api.same identity template then (
              mounted.lend () [@u];
              Option.iter
                (fun holes ->
                  template.lent <- None;
                  refresh mounted.scope holes [@u])
                template.lent;
              Some live)
            else None
        | _ -> None);
  }

let static markup =
  {
    shape = Single;
    build = (fun [@u] position -> place position (!medium.root position markup [@u]) (fresh 0) (Static_state markup));
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Static_state shown when Dom_api.same shown markup -> Some live
        | _ -> None);
  }

let check scope index value =
  let (Input remembered) = Array.unsafe_get scope.deps index in
  if Dom_api.same remembered value then Unchanged
  else (
    Array.unsafe_set scope.deps index (Input value);
    Changed)

module Hole = struct
  type text = { node : Dom.text; mutable value : string }
  type attr = { element : Dom.element; name : string; mutable value : string option }
  (** What else a property write updates: React keeps an input's [value] attribute in step. *)
  type echo = Alone | Default_value

  type prop = { element : Dom.element; name : string; mutable value : input; echo : echo }
  type style = { element : Dom.element; mutable value : (string * string) list }
  type classes = { element : Dom.element; mutable value : string list }
  type handler = { element : Dom.element; key : string }
  type reference = { scope : scope; element : Dom.element; mutable callback : Dom.element -> unit }
  type content = {
    position : Template.position;
    mutable source : child;
    mutable live : live;
    mutable range : range;
  }
end

let text_hole position =
  let node = !medium.text position None [@u] in
  ({ node; value = Dom_api.data (Dom_api.to_node node) } : Hole.text)

let set_text (hole : Hole.text) value =
  if not (Dom_api.same hole.value value) then (
    Dom_api.set_data hole.node value;
    hole.value <- value)

let attr_hole element name : Hole.attr = { element; name; value = None }

let set_attr (hole : Hole.attr) value =
  if not (Option.equal Dom_api.same hole.value value) then (
    (match value with
    | Some value -> Dom_api.set_attribute hole.element hole.name value
    | None -> Dom_api.remove_attribute hole.element hole.name);
    hole.value <- value)

let prop_hole element name : Hole.prop = { element; name; value = unset; echo = Alone }

let set_prop (hole : Hole.prop) value =
  let (Input old) = hole.value in
  if not (Dom_api.same old value) then (
    Dom_api.set_property hole.element hole.name value;
    (match hole.echo with
    | Alone -> ()
    | Default_value -> (
        match (Dom_api.property hole.element "type" : string) with
        | "number" when Dom_api.same (Dom_api.active_element Dom_api.document) (Js.Null.return hole.element) -> ()
        | _ -> Dom_api.set_property hole.element "defaultValue" value));
    hole.value <- Input value)

let controlled_hole element name =
  let hole : Hole.prop =
    {
      element;
      name;
      value = unset;
      echo =
        (match (name, Dom_api.node_name (Dom_api.to_node element)) with
        | "value", ("INPUT" | "TEXTAREA") -> Default_value
        | _ -> Alone);
    }
  in
  let restore _ =
    Dom_api.queue_microtask (fun () ->
        Dom_api.queue_microtask (fun () ->
            let (Input value) = hole.value in
            if not (Dom_api.same (Dom_api.property element name) value) then
              Dom_api.set_property element name value))
  in
  Dom_api.add_listener element "input" restore;
  Dom_api.add_listener element "change" restore;
  hole

let style_hole element : Hole.style = { element; value = [] }

let set_style (hole : Hole.style) value =
  if
    not
      (List.equal
         (fun (name, value) (name', value') -> Dom_api.same name name' && Dom_api.same value value')
         hole.value value)
  then (
    let style = Dom_api.style hole.element in
    List.iter (fun (name, _) -> Dom_api.set_style style name "") hole.value;
    List.iter (fun (name, value) -> Dom_api.set_style style name value) value;
    (match value with [] -> Dom_api.remove_attribute hole.element "style" | _ :: _ -> ());
    hole.value <- value)

let classes_hole element : Hole.classes = { element; value = [] }

let set_classes (hole : Hole.classes) value =
  if not (List.equal Dom_api.same hole.value value) then (
    let classes = Dom_api.class_list hole.element in
    List.iter (Dom_api.remove_class classes) hole.value;
    List.iter (Dom_api.add_class classes) value;
    (match value with [] -> Dom_api.remove_attribute hole.element "class" | _ :: _ -> ());
    hole.value <- value)

let handler_hole scope element event =
  let key, registration = Events.bind element event in
  (match registration with
  | Events.Delegated -> ()
  | Events.Direct remove -> scope.cleanups <- (fun [@u] () -> remove ()) :: scope.cleanups);
  ({ element; key } : Hole.handler)

let set_handler (hole : Hole.handler) next = Events.set hole.element hole.key next

let reference_hole scope element =
  let hole : Hole.reference = { scope; element; callback = ignore } in
  scope.references <- (fun [@u] () -> hole.callback hole.element) :: scope.references;
  hole

let set_reference (hole : Hole.reference) callback =
  if not (Dom_api.same hole.callback callback) then (
    hole.callback <- callback;
    match hole.scope.placement with Placed -> callback hole.element | Detached -> ())

let content_hole scope (position : Template.position) =
  let range = !medium.opened position [@u] in
  let hole : Hole.content = { position; source = empty; live = nothing; range } in
  scope.cleanups <- (fun [@u] () -> hole.live.release () [@u]) :: scope.cleanups;
  hole

let set_content (hole : Hole.content) child =
  match (hole.range, Dom_api.same hole.source child) with
  | Settled, true -> ()
  | Settled, false ->
      hole.live <- update hole.position hole.live child;
      hole.source <- child
  | Closing_at closing, _ ->
      hole.live <- child.build { hole.position with before = Js.Null.return closing } [@u];
      hole.source <- child;
      hole.range <- !medium.closed hole.position.parent closing [@u]

(** A hole of a hand-written template. *)
type site =
  | Text_hole of Hole.text
  | Attr_hole of Hole.attr
  | Prop_hole of Hole.prop
  | Style_hole of Hole.style
  | Class_hole of Hole.classes
  | Handler_hole of Hole.handler
  | Reference_hole of Hole.reference
  | Content_hole of Hole.content


let tpl_instance markup values =
  let mismatch () = Js.Exn.raiseError "melhole: hole value does not match its template hole" in
  let refresh sites values =
    if Array.length sites <> Array.length values then
      Js.Exn.raiseError
        ("melhole: template expects "
        ^ string_of_int (Array.length sites)
        ^ " hole values, got "
        ^ string_of_int (Array.length values));
    Array.iter2
      (fun site value ->
        match (site, value) with
        | Text_hole hole, HText value -> set_text hole value
        | Attr_hole hole, HAttr value -> set_attr hole value
        | Prop_hole hole, HProp value -> set_prop hole value
        | Style_hole hole, HStyle value -> set_style hole value
        | Class_hole hole, HClass value -> set_classes hole value
        | Handler_hole hole, HHandler value -> set_handler hole value
        | Reference_hole hole, HRef value -> set_reference hole value
        | Content_hole hole, HChild value -> set_content hole value
        | ( ( Text_hole _ | Attr_hole _ | Prop_hole _ | Style_hole _ | Class_hole _
            | Handler_hole _ | Reference_hole _ | Content_hole _ ),
            _ ) ->
            mismatch ())
      sites values
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root = !medium.root position markup [@u] in
        let scope = fresh 0 in
        let sites =
          Array.map
            (fun (site : (Dom.element, Template.position) Template.hole) ->
              match site with
              | Text at -> Text_hole (text_hole at)
              | Child at -> Content_hole (content_hole scope at)
              | Attr (element, name) -> Attr_hole (attr_hole element name)
              | Prop (element, name) -> Prop_hole (prop_hole element name)
              | Handler (element, event) -> Handler_hole (handler_hole scope element event)
              | Ref element -> Reference_hole (reference_hole scope element)
              | Style element -> Style_hole (style_hole element)
              | Class_list element -> Class_hole (classes_hole element))
            (Template.locate markup (Dom_api.to_node root))
        in
        refresh sites values;
        place position root scope (Paths_state (markup, refresh sites)));
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Paths_state (shown, write) when Dom_api.same shown markup ->
            write values;
            Some live
        | _ -> None);
  }

let mount child ~parent ~before =
  let position =
    {
      Template.parent = Dom_api.to_node parent;
      before = Js.Null.fromOption (Option.map Dom_api.to_node before);
    }
  in
  { position; source = child; live = child.build position [@u] }

let patch mounted child =
  if not (Dom_api.same mounted.source child) then (
    mounted.live <- update mounted.position mounted.live child;
    mounted.source <- child)

let unmount mounted =
  remove mounted.position mounted.live;
  mounted.live <- nothing;
  mounted.source <- empty

let host ~build ~patch = { shape = Single; build; patch }
let build child position = child.build position [@u]
let live hosted ~each ~front ~release = { state = Hosted hosted; each; front; release }
let hosted live = match live.state with Hosted hosted -> Some hosted | _ -> None
