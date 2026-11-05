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
      children : child array;  (** What each item rendered to. *)
      keys : string array;
      outer : input array;  (** What the rendering reads besides the item. *)
      projections : input array array;  (** Per row, the parts of its rendering that read [outer]. *)
      lives : live array;
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
}

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

let nothing =
  { state = Empty_state; each = (fun [@u] _ -> ()); front = (fun [@u] after -> after); release = (fun [@u] () -> ()) }

let insert (position : Template.position) node =
  Dom_api.insert_before position.parent node position.before

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

let backwards count (position : Template.position) make =
  let result = Array.make count nothing in
  let rec fill i before =
    if i >= 0 then (
      let live = make i { position with before } [@u] in
      Array.unsafe_set result i live;
      fill (i - 1) (live.front before [@u]))
  in
  fill (count - 1) position.before;
  result

let text value =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let node = Dom_api.create_text_node Dom_api.document value in
        insert position node;
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
        let items = Array.of_list (List.map (fun child -> child.build position [@u]) children) in
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
        let items = Array.map (fun child -> child.build position [@u]) children in
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
            in
            Some (group (Keyed_state (keys, items)) items)
        | _ -> None);
  }

let with_key key child = { child with shape = Entry (key, child) }

let each items render project outer =
  let count = Array.length items in
  let key index child =
    match child.shape with
    | Entry (key, _) -> key
    | Single | Nothing | Many _ | Entries _ | Deferred _ -> string_of_int index
  in
  let rendered index item =
    let child = render item [@u] in
    (child, key index child)
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let children = Array.make count empty and keys = Array.make count "" in
        Array.iteri
          (fun index item ->
            let child, key = rendered index item in
            Array.unsafe_set children index child;
            Array.unsafe_set keys index key)
          items;
        let projections = Array.map (fun item -> project item [@u]) items in
        let lives = Array.map (fun child -> child.build position [@u]) children in
        group (Each_state { items; children; keys; outer; projections; lives }) lives);
    patch =
      (fun [@u] position live ->
        match live.state with
        | Each_state old ->
            let old_count = Array.length old.items in
            let children = Array.make count empty and keys = Array.make count "" in
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
                  Array.unsafe_set children index (Array.unsafe_get old.children from);
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
                  Array.unsafe_set children index child;
                  Array.unsafe_set keys index key;
                  Array.unsafe_set projections index
                    (match projected with Some projection -> projection | None -> project item [@u])))
              items;
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
                    let child = Array.unsafe_get children index in
                    if not (Dom_api.same child (Array.unsafe_get old.children index)) then
                      Array.unsafe_set lives index
                        (update
                           { position with before = ahead (index + 1) }
                           (Array.unsafe_get old.lives index)
                           child);
                    refresh (index - 1))
                in
                refresh (count - 1);
                Some (group (Each_state { items; children; keys; outer; projections; lives }) lives)
            | _, (`Same | `Moved) ->
                let plan = Reconcile.plan ~old:old.keys ~next:keys in
                (match (List.compare_length_with plan.dropped old_count, extent position live) with
                | 0, Whole ->
                    Dom_api.set_text_content position.parent "";
                    live.release () [@u]
                | _, (Whole | Part) ->
                    List.iter (fun i -> remove position (Array.unsafe_get old.lives i)) plan.dropped);
                let carried at i j =
                  let child = Array.unsafe_get children j in
                  match Dom_api.same child (Array.unsafe_get old.children i) with
                  | true -> Array.unsafe_get old.lives i
                  | false -> update at (Array.unsafe_get old.lives i) child
                in
                let lives =
                  backwards count position (fun [@u] j at ->
                      match Array.unsafe_get plan.steps j with
                      | Create -> (Array.unsafe_get children j).build at [@u]
                      | Keep i -> carried at i j
                      | Move i ->
                          let live = carried at i j in
                          live.each (fun [@u] node -> insert at node) [@u];
                          live)
                in
                Some (group (Each_state { items; children; keys; outer; projections; lives }) lives))
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
    build = (fun [@u] _ -> held (child.build inside [@u]));
    patch =
      (fun [@u] _ live ->
        match live.state with
        | Portal_state (target, inner) when Dom_api.same target container ->
            Some (held (update inside inner child))
        | _ -> None);
  }

let place position root scope state =
  insert position root;
  scope.placement <- Placed;
  List.iter (fun reference -> reference () [@u]) (List.rev scope.references);
  {
    state;
    each = (fun [@u] visit -> visit (Dom_api.to_node root) [@u]);
    front = (fun [@u] _ -> Js.Null.return (Dom_api.to_node root));
    release = (fun [@u] () -> List.iter (fun cleanup -> cleanup () [@u]) (List.rev scope.cleanups));
  }

let fresh count = { deps = Array.make count (Input (ref ())); cleanups = []; references = []; placement = Detached }

let template markup holes = { markup; holes; lent = None }

let compiled template count refresh =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root = Template.clone template.markup in
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
    build = (fun [@u] position -> place position (Template.clone markup) (fresh 0) (Static_state markup));
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
  type prop = { element : Dom.element; name : string; mutable value : input }
  type style = { element : Dom.element; mutable value : (string * string) list }
  type classes = { element : Dom.element; mutable value : string list }
  type handler = { binding : Events.t }
  type reference = { scope : scope; element : Dom.element; mutable callback : Dom.element -> unit }
  type content = { position : Template.position; mutable source : child; mutable live : live }
end

let text_hole position =
  let node = Dom_api.create_text_node Dom_api.document "" in
  insert position node;
  ({ node; value = "" } : Hole.text)

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

let prop_hole element name : Hole.prop = { element; name; value = Input (ref ()) }

let set_prop (hole : Hole.prop) value =
  let (Input old) = hole.value in
  if not (Dom_api.same old value) then (
    Dom_api.set_property hole.element hole.name value;
    hole.value <- Input value)

let controlled_hole element name =
  let hole = prop_hole element name in
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
  let binding = Events.bind element event ignore in
  scope.cleanups <- (fun [@u] () -> Events.release binding) :: scope.cleanups;
  ({ binding } : Hole.handler)

let set_handler (hole : Hole.handler) next = Events.set hole.binding next

let reference_hole scope element =
  let hole : Hole.reference = { scope; element; callback = ignore } in
  scope.references <- (fun [@u] () -> hole.callback hole.element) :: scope.references;
  hole

let set_reference (hole : Hole.reference) callback =
  if not (Dom_api.same hole.callback callback) then (
    hole.callback <- callback;
    match hole.scope.placement with Placed -> callback hole.element | Detached -> ())

let content_hole scope position =
  let hole : Hole.content = { position; source = empty; live = nothing } in
  scope.cleanups <- (fun [@u] () -> hole.live.release () [@u]) :: scope.cleanups;
  hole

let set_content (hole : Hole.content) child =
  if not (Dom_api.same hole.source child) then (
    hole.live <- update hole.position hole.live child;
    hole.source <- child)
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
        let root = Template.clone markup in
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
