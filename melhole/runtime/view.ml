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
  | Mounted_row : { root : Dom.element; scope : scope; holes : 'holes; witness : 'holes Witness.t } -> mounted_row

(** What a mounted child is, so that the next child can tell whether it can patch it. *)
type state =
  | Empty_state
  | Text_state of { node : Dom.text; mutable value : string }
  | List_state of live array
  | Keyed_state of string array * live array
  | Memo_state of input array * live
  | One_keyed_state of string * live  (** A child with a key outside a list: another key is another child. *)
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
  witness : 'holes Witness.t;  (** Proves that holes made by this template have its type. *)
  adopt : 'holes adoptable;
}

(** How a compiled template takes over a server's nodes: given its holes on a first, unplaced
    clone, it makes its holes on the server's element. *)
and 'holes adoptable = Not_hydratable | Adopts of (scope -> Dom.node -> 'holes -> 'holes[@u])

(** One row of a compiled loop before it is mounted: its template, how to write its holes, its key. *)

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

let nothing =
  { state = Empty_state; each = (fun [@u] _ -> ()); front = (fun [@u] after -> after); release = (fun [@u] () -> ()) }

let insert (position : Template.position) node =
  Dom_api.insert_before position.parent node position.before

external moving : Dom.node -> (Dom.node -> Dom.node Js.null -> unit) Js.undefined = "moveBefore" [@@mel.get]
external move_before : Dom.node -> Dom.node -> Dom.node Js.null -> unit = "moveBefore" [@@mel.send]

let move parent node before =
  match Js.Undefined.toOption (moving parent) with
  | None -> Dom_api.insert_before parent node before
  | Some _ -> (
      try move_before parent node before with Js.Exn.Error _ -> Dom_api.insert_before parent node before)

(** Whether a content hole shows its children, or only remembers them for hydration's first pass. *)
type range = Settled | Collecting

(** Where a medium's nodes come from. *)
type adoption = Fresh_nodes | Server_nodes

(** How children reach the DOM: built in the browser, or adopted from the server's HTML. *)
type medium = {
  adoption : adoption;
  text : (Template.position -> string option -> Dom.text[@u]);  (** [None]: the text is written later. *)
  root : (Template.position -> Template.t -> Dom.element[@u]);
  placed : (Template.position -> Dom.element -> unit[@u]);
  opened : (Template.position -> range[@u]);  (** What a new content hole does with its children. *)
  built :
    'made.
    (int ->
    Template.position ->
    (int -> Template.position -> 'made[@u]) ->
    ('made -> Dom.node Js.null -> Dom.node Js.null[@u]) ->
    'made array[@u]);
      (** Makes children in order; the last argument gives the node after one, from where it started. *)
  aside : ((unit -> live[@u]) -> live[@u]);  (** Children that live elsewhere, like a portal's. *)
}

let browser =
  {
    adoption = Fresh_nodes;
    text =
      (fun [@u] position value ->
        let node = Dom_api.create_text_node Dom_api.document (Option.value ~default:"" value) in
        insert position node;
        node);
    root = (fun [@u] _ markup -> Template.clone markup);
    placed = (fun [@u] position root -> insert position root);
    opened = (fun [@u] _ -> Settled);
    built =
      (fun [@u] count position make _ ->
        match count with
        | 0 -> [||]
        | _ ->
            let made = Array.make count (make 0 position [@u]) in
            for index = 1 to count - 1 do
              Array.unsafe_set made index (make index position [@u])
            done;
            made);
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
  let count = Array.length old in
  let rec scan index =
    match index = count with
    | true -> Unchanged
    | false ->
        let (Input a) = Array.unsafe_get old index in
        let (Input b) = Array.unsafe_get next index in
        if Dom_api.same a b then scan (index + 1) else Changed
  in
  match count = Array.length next with true -> scan 0 | false -> Changed

type 'value memory = { mutable seen : input array; mutable kept : 'value option }

let memory () = { seen = [||]; kept = None }

let remember memory inputs compute =
  match (memory.kept, compare_inputs memory.seen inputs) with
  | Some kept, Unchanged -> kept
  | Some _, Changed | None, (Changed | Unchanged) ->
      let value = compute () in
      memory.seen <- inputs;
      memory.kept <- Some value;
      value

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

let after_of =
 fun [@u] (live : live) start ->
  let last = ref Js.null in
  live.each (fun [@u] node -> last := Js.Null.return node) [@u];
  match Js.Null.toOption !last with None -> start | Some node -> Dom_api.following_sibling node

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
        let items = !medium.built (Array.length children) position (fun [@u] index at -> (Array.unsafe_get children index).build at [@u]) after_of [@u] in
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
        let items = !medium.built (Array.length children) position (fun [@u] index at -> (Array.unsafe_get children index).build at [@u]) after_of [@u] in
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
                      live.each (fun [@u] node -> move at.parent node at.before) [@u];
                      live)
                front_of
            in
            Some (group (Keyed_state (keys, items)) items)
        | _ -> None);
  }

let with_key key child =
  let keep inner = { inner with state = One_keyed_state (key, inner) } in
  {
    shape = Entry (key, child);
    build = (fun [@u] position -> keep (child.build position [@u]));
    patch =
      (fun [@u] position live ->
        match live.state with
        | One_keyed_state (shown, inner) when String.equal shown key -> Option.map keep (child.patch position inner [@u])
        | _ -> None);
  }

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
            after_of [@u]
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
                          live.each (fun [@u] node -> move at.parent node at.before) [@u];
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

let let_go scope =
  match scope.cleanups with
  | [] -> ()
  | cleanups -> List.iter (fun cleanup -> cleanup () [@u]) (List.rev cleanups)

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

let template markup holes = { markup; holes; lent = None; witness = Witness.make (); adopt = Not_hydratable }

let hydratable markup holes adopt = { markup; holes; lent = None; witness = Witness.make (); adopt = Adopts adopt }

let mismatch () = Js.Exn.raiseError "melhole: the HTML from the server does not match the view"

let found node =
  match Js.Null.toOption node with
  | None -> "nothing"
  | Some node -> (
      match Dom_api.node_type node with
      | 1 -> "<" ^ String.lowercase_ascii (Dom_api.node_name node) ^ ">"
      | 3 -> "the text \"" ^ Dom_api.data node ^ "\""
      | _ -> "a comment")

let expected what node =
  Js.Exn.raiseError ("melhole: the HTML from the server does not match the view: expected " ^ what ^ ", found " ^ found node)

let collecting = { browser with opened = (fun [@u] _ -> Collecting) }

let adopted template slots refresh (position : Template.position) =
  match template.adopt with
  | Not_hydratable -> mismatch ()
  | Adopts adopt ->
      let root = !medium.root position template.markup [@u] in
      let serving = !medium in
      medium := collecting;
      let clone = Template.clone template.markup in
      let first = fresh slots in
      let collected = template.holes first (Dom_api.to_node clone) [@u] in
      refresh first collected [@u];
      medium := serving;
      let scope = fresh slots in
      let holes = adopt scope (Dom_api.to_node root) collected [@u] in
      refresh scope holes [@u];
      (root, scope, holes)

let rows (type holes item) (items : item array) (template : holes template) slots
    (refresh : (scope -> holes -> item -> unit)[@u]) (key : (item -> string)[@u]) project outer =
  let count = Array.length items in
  let mount (position : Template.position) item =
    match !medium.adoption with
    | Fresh_nodes ->
        let root = !medium.root position template.markup [@u] in
        let scope = fresh slots in
        let holes = template.holes scope (Dom_api.to_node root) [@u] in
        refresh scope holes item [@u];
        settle position root scope;
        Mounted_row { root; scope; holes; witness = template.witness }
    | Server_nodes ->
        let root, scope, holes =
          adopted template slots (fun [@u] scope holes -> refresh scope holes item [@u]) position
        in
        settle position root scope;
        Mounted_row { root; scope; holes; witness = template.witness }
  in
  let root_of (Mounted_row row) = Dom_api.to_node row.root in
  let after = fun [@u] row _ -> Dom_api.following_sibling (root_of row) in
  let shown items keys projections rows =
    {
      state = Rows_state { items; keys; outer; projections; rows };
      each = (fun [@u] visit -> Array.iter (fun row -> visit (root_of row) [@u]) rows);
      front =
        (fun [@u] after ->
          match Array.length rows with 0 -> after | _ -> Js.Null.return (root_of (Array.unsafe_get rows 0)));
      release =
        (fun [@u] () ->
          for index = 0 to Array.length rows - 1 do
            let (Mounted_row row) = Array.unsafe_get rows index in
            let_go row.scope
          done);
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
              let item = Array.unsafe_get items index in
              Array.unsafe_set keys index (key item [@u]);
              mount at item)
            after [@u]
        in
        let projections = Array.make count [||] in
        for index = 0 to count - 1 do
          Array.unsafe_set projections index (project (Array.unsafe_get items index) [@u])
        done;
        shown items keys projections rows);
    patch =
      (fun [@u] position live ->
        match live.state with
        | Rows_state old ->
            let old_count = Array.length old.items in
            let next = Array.make count (Kept 0) and keys = Array.make count "" in
            let projections = Array.make count [||] in
            let outer_change = compare_inputs old.outer outer in
            let fits from item projected =
              from >= 0 && from < old_count
              && Dom_api.same (Array.unsafe_get old.items from) item
              &&
              match projected with
              | None -> true
              | Some projection -> (
                  match compare_inputs (Array.unsafe_get old.projections from) projection with
                  | Unchanged -> true
                  | Changed -> false)
            in
            let reuse index from projected =
              Array.unsafe_set next index (Kept from);
              Array.unsafe_set keys index (Array.unsafe_get old.keys from);
              Array.unsafe_set projections index
                (match projected with Some projection -> projection | None -> Array.unsafe_get old.projections from)
            in
            let shift = ref 0 in
            for index = 0 to count - 1 do
              let item = Array.unsafe_get items index in
              let projected = match outer_change with Unchanged -> None | Changed -> Some (project item [@u]) in
              let from = index + !shift in
              if fits from item projected then reuse index from projected
              else if fits (from + 1) item projected then (
                incr shift;
                reuse index (from + 1) projected)
              else if fits (from - 1) item projected then (
                decr shift;
                reuse index (from - 1) projected)
              else (
                Array.unsafe_set next index (Fresh ());
                Array.unsafe_set keys index (key item [@u]);
                Array.unsafe_set projections index
                  (match projected with Some projection -> projection | None -> project item [@u]))
            done;
            let drawn index = Array.unsafe_get items index in
            let refreshed (Mounted_row row as mounted) item =
              match Witness.equal row.witness template.witness with
              | Some Equal ->
                  refresh row.scope row.holes item [@u];
                  mounted
              | None ->
                  let made =
                    mount { position with before = Dom_api.following_sibling (Dom_api.to_node row.root) } item
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
                let rows = Array.copy old.rows in
                for index = 0 to count - 1 do
                  match Array.unsafe_get next index with
                  | Kept from when from = index -> ()
                  | Kept _ | Fresh _ -> Array.unsafe_set rows index (refreshed (Array.unsafe_get rows index) (drawn index))
                done;
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
                    for index = 0 to old_count - 1 do
                      let (Mounted_row row) = Array.unsafe_get old.rows index in
                      let_go row.scope
                    done
                | `Part -> List.iter (fun i -> drop position (Array.unsafe_get old.rows i)) plan.dropped);
                let carried i j =
                  match Array.unsafe_get next j with
                  | Kept from when from = i -> Array.unsafe_get old.rows i
                  | Kept _ | Fresh _ -> refreshed (Array.unsafe_get old.rows i) (drawn j)
                in
                let placed j before =
                  match Array.unsafe_get plan.steps j with
                  | Create -> mount { position with before } (drawn j)
                  | Keep i -> carried i j
                  | Move i ->
                      let moved = carried i j in
                      move position.parent (root_of moved) before;
                      moved
                in
                let rows =
                  match count with
                  | 0 -> [||]
                  | _ ->
                      let last = placed (count - 1) position.before in
                      let rows = Array.make count last in
                      let before = ref (Js.Null.return (root_of last)) in
                      for j = count - 2 downto 0 do
                        let row = placed j !before in
                        Array.unsafe_set rows j row;
                        before := Js.Null.return (root_of row)
                      done;
                      rows
                in
                Some (shown items keys projections rows))
        | _ -> None);
  }


let compiled template count refresh =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root, scope, holes =
          match !medium.adoption with
          | Fresh_nodes ->
              let root = !medium.root position template.markup [@u] in
              let scope = fresh count in
              let holes = template.holes scope (Dom_api.to_node root) [@u] in
              refresh scope holes [@u];
              (root, scope, holes)
          | Server_nodes -> adopted template count refresh position
        in
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
  (** What else a property write updates: React keeps an input's [value] attribute in step, and
      gives a checkbox the [checked] attribute it had when mounted. *)
  type echo = Alone | Default_value | Default_checked_at_mount

  type prop = { element : Dom.element; name : string; mutable value : input; echo : echo }
  type style = { element : Dom.element; mutable value : (string * string) list }
  type classes = { element : Dom.element; mutable value : string list }
  type handler = { element : Dom.element; key : string }
  type reference = { scope : scope; element : Dom.element; mutable callback : Dom.element -> unit }
  type content = {
    position : Template.position;
    mutable source : child;
    mutable live : live;
    range : range;
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
        | _ -> Dom_api.set_property hole.element "defaultValue" value)
    | Default_checked_at_mount -> (
        match Dom_api.same hole.value unset with
        | true -> Dom_api.set_property hole.element "defaultChecked" value
        | false -> ()));
    hole.value <- Input value)

external html_of : < __html : string > Js.t -> string = "__html" [@@mel.get]

let restores = ref []

let restore_controlled () =
  let pending = !restores in
  restores := [];
  List.iter (fun restore -> restore ()) (List.rev pending)

let controlled_hole element name =
  let hole : Hole.prop =
    {
      element;
      name;
      value = unset;
      echo =
        (match (name, Dom_api.node_name (Dom_api.to_node element)) with
        | "value", ("INPUT" | "TEXTAREA") -> Default_value
        | "checked", "INPUT" -> Default_checked_at_mount
        | _ -> Alone);
    }
  in
  let restore _ =
    restores :=
      (fun () ->
        let (Input value) = hole.value in
        if not (Dom_api.same (Dom_api.property element name) value) then Dom_api.set_property element name value)
      :: !restores;
    Dom_api.queue_microtask (fun () -> Dom_api.queue_microtask restore_controlled)
  in
  Dom_api.add_listener element "input" restore;
  Dom_api.add_listener element "change" restore;
  Dom_api.set_handler element Events.controlled restore;
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

let text_at position = !medium.text position None [@u]

let put_text scope slot node value =
  let (Input last) = Array.unsafe_get scope.deps slot in
  if not (Dom_api.same last value) then (
    Dom_api.set_data node value;
    Array.unsafe_set scope.deps slot (Input value))

let put_sole_text scope slot element value =
  let (Input last) = Array.unsafe_get scope.deps slot in
  if not (Dom_api.same last value) then (
    (match Dom_api.first_child element with
    | Some node when Dom_api.node_type node = 3 -> Dom_api.set_data (Dom_api.unsafe_text node) value
    | Some _ | None -> Dom_api.set_text_content (Dom_api.to_node element) value);
    Array.unsafe_set scope.deps slot (Input value))

let put_attr scope slot element name value =
  let (Input last) = Array.unsafe_get scope.deps slot in
  if not (Dom_api.same last value) then (
    (match value with
    | Some value -> Dom_api.set_attribute element name value
    | None -> Dom_api.remove_attribute element name);
    Array.unsafe_set scope.deps slot (Input value))

let listen scope element event =
  let key, registration = Events.bind element event in
  (match registration with
  | Events.Delegated -> ()
  | Events.Direct remove -> scope.cleanups <- (fun [@u] () -> remove ()) :: scope.cleanups);
  key

let put_handler element key next = Events.set element key next

(** A handler site compiled once: what it does with the data its element carries and the dispatch. *)
type ('data, 'dispatch) link = {
  body : ('data -> 'dispatch -> Dom.event -> unit[@u]);
  data_key : string;  (** The element property that carries this site's data. *)
  mutable sent : 'dispatch option;  (** The dispatch the shared handler was made for. *)
  mutable handler : Dom.event -> unit;
}

let sites = ref 0

let link body =
  incr sites;
  {
    body;
    data_key = "$$melhole:data" ^ string_of_int !sites;
    sent = None;
    handler = ignore;
  }

let put_linked element key link dispatch data =
  Dom_api.set_property element link.data_key data;
  (match link.sent with
  | Some sent when Dom_api.same sent dispatch -> ()
  | Some _ | None ->
      link.sent <- Some dispatch;
      link.handler <-
        (fun event -> link.body (Dom_api.property (Dom_api.current_target event) link.data_key) dispatch event [@u]));
  Events.set element key link.handler

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
  | Collecting, _ -> hole.source <- child

(** Where hydration has got to among one parent's children. *)
type cursor = { parent : Dom.node; mutable at : Dom.node Js.null }

let inside node = { parent = node; at = Dom_api.first_child_or_end node }

let separated node =
  match Js.Null.toOption node with
  | Some found when Dom_api.node_type found = 8 && String.equal (Dom_api.data found) " " ->
      Dom_api.following_sibling found
  | Some _ | None -> node

let adopt_element cursor tag =
  match Js.Null.toOption cursor.at with
  | Some node
    when Dom_api.node_type node = 1
         && String.equal (String.lowercase_ascii (Dom_api.node_name node)) (String.lowercase_ascii tag) ->
      cursor.at <- Dom_api.following_sibling node;
      node
  | Some _ | None -> expected ("<" ^ tag ^ ">") cursor.at

let adopt_static cursor text =
  match Js.Null.toOption (separated cursor.at) with
  | Some node when Dom_api.node_type node = 3 && String.equal (Dom_api.data node) text ->
      cursor.at <- Dom_api.following_sibling node
  | Some _ | None -> expected ("the text \"" ^ text ^ "\"") (separated cursor.at)

let anchor cursor =
  match Js.Null.toOption cursor.at with
  | Some node when Dom_api.node_type node = 8 ->
      cursor.at <- Dom_api.following_sibling node;
      node
  | Some _ | None ->
      let made = Dom_api.to_node (Dom_api.create_text_node Dom_api.document "") in
      Dom_api.insert_before cursor.parent made cursor.at;
      made

let adopt_anchor cursor = ignore (anchor cursor)

let adopt_text cursor value =
  match (value, Js.Null.toOption (separated cursor.at)) with
  | "", _ ->
      let node = Dom_api.create_text_node Dom_api.document "" in
      Dom_api.insert_before cursor.parent node cursor.at;
      node
  | _, Some node when Dom_api.node_type node = 3 ->
      cursor.at <- Dom_api.following_sibling node;
      if not (String.equal (Dom_api.data node) value) then Dom_api.set_data (Dom_api.unsafe_text node) value;
      Dom_api.unsafe_text node
  | _, (Some _ | None) -> expected ("the text \"" ^ value ^ "\"") (separated cursor.at)

(** What follows a content hole in its template: whatever comes next, or an anchor the hole ends at. *)
type ending = Before_next | Before_anchor

let adopt_content scope cursor child ending =
  let live = child.build { Template.parent = cursor.parent; before = cursor.at } [@u] in
  cursor.at <- after_of live cursor.at [@u];
  let before = match ending with Before_next -> cursor.at | Before_anchor -> Js.Null.return (anchor cursor) in
  let hole : Hole.content = { position = { parent = cursor.parent; before }; source = child; live; range = Settled } in
  scope.cleanups <- (fun [@u] () -> hole.live.release () [@u]) :: scope.cleanups;
  hole

let adopt_finish cursor =
  let rec rest node =
    match Js.Null.toOption node with
    | None -> ()
    | Some found -> (
        match (Dom_api.node_type found, Dom_api.data found, Dom_api.node_name cursor.parent) with
        | 8, _, _ | 3, "", _ | 3, _, "TEXTAREA" -> rest (Dom_api.following_sibling found)
        | _, _, _ -> expected "the end of an element" node)
  in
  rest cursor.at

let collected (hole : Hole.content) = hole.source
let collected_text node = Dom_api.data (Dom_api.to_node node)

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
  let adopt_instance (position : Template.position) =
    let root = !medium.root position markup [@u] in
    let scope = fresh 0 in
    let paths = Template.paths markup in
    let elements = ref [] in
    let adopted = Array.make (Array.length paths) None in
    let slot cursor index ending =
      Array.unsafe_set adopted index
        (match (Array.unsafe_get paths index, Array.unsafe_get values index) with
        | Text _, HText text -> Some (Text_hole { node = adopt_text cursor text; value = text })
        | Child _, HChild child -> Some (Content_hole (adopt_content scope cursor child ending))
        | _, _ -> mismatch ())
    in
    let rec walk shape server path =
      elements := (path, server) :: !elements;
      let cursor = inside (Dom_api.to_node server) in
      let rec step node index =
        let ending =
          match Option.map (fun current -> (Dom_api.node_type current, Dom_api.data current)) node with
          | Some ((3, "") | (8, _)) -> Before_anchor
          | Some _ | None -> Before_next
        in
        let taken = ref Before_next in
        Array.iteri
          (fun at hole ->
            match hole with
            | Template.Text (parent, before) when before = index && List.equal Int.equal parent path ->
                slot cursor at Before_next
            | Child (parent, before) when before = index && List.equal Int.equal parent path ->
                slot cursor at ending;
                taken := ending
            | Text _ | Child _ | Attr _ | Prop _ | Handler _ | Ref _ | Style _ | Class_list _ -> ())
          paths;
        Option.iter
          (fun current ->
            (match (Dom_api.node_type current, Dom_api.data current, !taken) with
            | 1, _, _ ->
                walk current (adopt_element cursor (Dom_api.node_name current)) (path @ [ index ])
            | 3, "", Before_anchor | 8, _, Before_anchor -> ()
            | 3, "", Before_next | 8, _, Before_next -> adopt_anchor cursor
            | _, text, _ -> adopt_static cursor text);
            step (Dom_api.next_sibling current) (index + 1))
          node
      in
      step (Dom_api.first_child (Dom_api.to_node shape)) 0;
      adopt_finish cursor
    in
    walk (Dom_api.to_node (Template.parsed markup)) (Dom_api.to_node root) [];
    let at path =
      Template.element
        (Option.get
           (List.find_map
              (fun (known, server) -> match List.equal Int.equal known path with true -> Some server | false -> None)
              !elements))
    in
    let sites =
      Array.mapi
        (fun index hole ->
          match (hole : (int list, int list * int) Template.hole) with
          | Text _ | Child _ -> Option.get (Array.unsafe_get adopted index)
          | Attr (path, name) -> Attr_hole (attr_hole (at path) name)
          | Prop (path, name) -> Prop_hole (prop_hole (at path) name)
          | Handler (path, event) -> Handler_hole (handler_hole scope (at path) event)
          | Ref path -> Reference_hole (reference_hole scope (at path))
          | Style path -> Style_hole (style_hole (at path))
          | Class_list path -> Class_hole (classes_hole (at path)))
        paths
    in
    refresh sites values;
    place position root scope (Paths_state (markup, refresh sites))
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        match !medium.adoption with
        | Server_nodes -> adopt_instance position
        | Fresh_nodes ->
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

let mount_over child ~parent =
  let parent = Dom_api.to_node parent in
  let position = { Template.parent; before = Js.null } in
  { position; source = child; live = child.build { position with before = Dom_api.first_child_or_end parent } [@u] }

let unmount mounted =
  remove mounted.position mounted.live;
  mounted.live <- nothing;
  mounted.source <- empty

let host ~build ~patch = { shape = Single; build; patch }
let build child position = child.build position [@u]
let live hosted ~each ~front ~release = { state = Hosted hosted; each; front; release }
let hosted live = match live.state with Hosted hosted -> Some hosted | _ -> None
