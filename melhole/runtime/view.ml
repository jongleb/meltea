(** When an effect runs after its instance is patched. *)
type timing =
  | Layout  (** Synchronously, before the browser paints. *)
  | Passive  (** After the whole flush, once the browser has painted. *)

(** Within one commit every cleanup runs before any effect, as in React. *)
type phase = Cleanup | Run

type child =
  | Text of string
  | Empty
  | List of child list
  | Keyed of (string * child) list
  | Key of string * child
  | Element of Template.t * hole array
  | Lazy of thunk
  | Component of Component.t * input array * (unit -> child)
  | Provide : 'value Context.t * 'value * child -> child
  | Portal of Dom.element * child
  | Memo of input array * (unit -> child)
  | Guarded of Template.t * int * (block -> unit)

and hole =
  | HText of string
  | HAttr of string option
  | HProp : 'a -> hole
  | HClass of string list
  | HStyle of (string * string) list
  | HHandler of (Dom.event -> unit)
  | HRef of (Dom.element -> unit)
  | HChild of child

(** A value of any type, compared physically. *)
and input = Input : 'a -> input [@@unboxed]

(** A render function with the arguments it is skipped for. *)
and thunk =
  | Lazy1 : ('a -> child) * 'a -> thunk
  | Lazy2 : ('a -> 'b -> child) * 'a * 'b -> thunk

(** What a mounted description owns in the DOM. *)
and live =
  | Text_live of Dom.text * string
  | Empty_live
  | List_live of live array
  | Keyed_live of string array * live array
  | Element_live of block
  | Lazy_live of thunk * live
  | Component_live of instance
  | Portal_live of Dom.element * live
  | Memo_live of input array * live

(** A mounted template: its root, its holes and the values its compiled refresh last read. *)
and block = {
  template : Template.t;
  root : Dom.element;
  bindings : binding array;
  deps : input array;
}

(** A template hole bound to its DOM site, with the value last written. *)
and binding =
  | Unbound_site of (Dom.element, Template.position) Template.hole
      (** Not filled yet: a compiled refresh fills every hole on mount. *)
  | Attr_site of {
      element : Dom.element;
      name : string;
      mutable value : string option;
    }
  | Prop_site of { element : Dom.element; name : string; mutable value : input }
  | Text_site of { node : Dom.text; mutable value : string }
  | Child_site of {
      position : Template.position;
      mutable source : child;
      mutable live : live;
    }
  | Handler_site of Events.t
  | Ref_site of { element : Dom.element; mutable callback : Dom.element -> unit }
  | Style_site of {
      element : Dom.element;
      mutable value : (string * string) list;
    }
  | Class_site of { element : Dom.element; mutable value : string list }

(** A mounted component: its hook slots, its output and its effects. *)
and instance = {
  parent : instance option;  (** The nearest enclosing instance. *)
  depth : int;
  slots : Hook.slots;
  kind : kind;
  mutable status : status;
  mutable output : live;
  mutable region : region;
  mutable pending : (timing * phase * (unit -> unit)) list;  (** Effects of the last render, newest first. *)
  mutable teardown : (unit -> unit) list;  (** Run on unmount, newest first. *)
}

and kind =
  | Renders of {
      component : Component.t;
      mutable inputs : input array;
      mutable render : unit -> child;
    }
  | Provides : {
      context : 'value Context.t;
      mutable value : 'value;
      mutable children : child;
      mutable consumers : instance list;
    }
      -> kind

and status = Clean | Dirty | Unmounted

(** Where an instance sits in the DOM when its output has no nodes. *)
and region =
  | Nodes  (** The output's own nodes locate it. *)
  | Marker of Dom.text  (** An empty text node holds its place. *)

(** Whether a value differs from the one a block remembers for it. *)
type change = Changed | Unchanged

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

(** Whether a flush is already queued. *)
type schedule = Idle | Queued

let rendering = ref None
let owner = ref None
let dirty = ref []
let layout_cleanups = ref []
let layout_runs = ref []
let passive_cleanups = ref []
let passive_runs = ref []
let render_schedule = ref Idle
let passive_schedule = ref Idle
let text value = Text value
let empty = Empty
let keyed entries = Keyed entries
let with_key key child = Key (key, child)
let tpl_instance template holes = Element (template, holes)
let lazy1 render a = Lazy (Lazy1 (render, a))
let lazy2 render a b = Lazy (Lazy2 (render, a, b))
let component definition inputs render = Component (definition, inputs, render)
let provide context value children = Provide (context, value, children)
let portal container child = Portal (container, child)
let memo inputs render = Memo (inputs, render)
let guarded template count refresh = Guarded (template, count, refresh)
let unset = Input (ref ())

let list children =
  let rec entries collected children =
    match children with
    | [] -> Some (List.rev collected)
    | Key (key, child) :: rest -> entries ((key, child) :: collected) rest
    | ( Text _ | Empty | List _ | Keyed _ | Element _ | Lazy _ | Component _
      | Provide _ | Portal _ | Memo _ | Guarded _ )
      :: _ ->
        None
  in
  match children with
  | [] -> List []
  | _ :: _ ->
      Option.fold ~none:(List children) ~some:keyed (entries [] children)

let within cell value run =
  let saved = !cell in
  cell := value;
  match run () with
  | result ->
      cell := saved;
      result
  | exception error ->
      cell := saved;
      raise error

let force thunk =
  match thunk with Lazy1 (render, a) -> render a | Lazy2 (render, a, b) -> render a b

let rec items child =
  match child with
  | Empty -> []
  | List children -> List.concat_map items children
  | Keyed entries -> List.map (fun (key, child) -> Key (key, child)) entries
  | Lazy thunk -> items (force thunk)
  | Memo (_, render) -> items (render ())
  | Text _ | Key _ | Element _ | Guarded _ | Component _ | Provide _ | Portal _ -> [ child ]

let insert (position : Template.position) node =
  Dom_api.insert_before position.parent node (Js.Null.fromOption position.before)

let restyle element old next =
  let style = Dom_api.style element in
  List.iter (fun (name, _) -> Dom_api.set_style style name "") old;
  List.iter (fun (name, value) -> Dom_api.set_style style name value) next;
  match next with [] -> Dom_api.remove_attribute element "style" | _ :: _ -> ()

let reclass element old next =
  let classes = Dom_api.class_list element in
  List.iter (Dom_api.remove_class classes) old;
  List.iter (Dom_api.add_class classes) next;
  match next with [] -> Dom_api.remove_attribute element "class" | _ :: _ -> ()

let mark_dirty instance =
  match instance.status with
  | Clean ->
      instance.status <- Dirty;
      dirty := instance :: !dirty
  | Dirty | Unmounted -> ()

let run_jobs jobs =
  List.iter
    (fun (instance, job) ->
      match instance.status with Unmounted -> () | Clean | Dirty -> job ())
    (List.rev jobs)

let run_phases cleanups runs =
  let pending_cleanups = !cleanups and pending_runs = !runs in
  cleanups := [];
  runs := [];
  run_jobs pending_cleanups;
  run_jobs pending_runs

let run_layout () = run_phases layout_cleanups layout_runs

let commit instance =
  List.iter
    (fun (timing, phase, job) ->
      match (timing, phase) with
      | Layout, Cleanup -> layout_cleanups := (instance, job) :: !layout_cleanups
      | Layout, Run -> layout_runs := (instance, job) :: !layout_runs
      | Passive, Cleanup -> passive_cleanups := (instance, job) :: !passive_cleanups
      | Passive, Run -> passive_runs := (instance, job) :: !passive_runs)
    (List.rev instance.pending);
  instance.pending <- []

let run instance =
  instance.status <- Clean;
  match instance.kind with
  | Renders definition ->
      Hook.rewind instance.slots;
      within rendering (Some instance) definition.render
  | Provides provision -> provision.children

let rec each_node visit live =
  match live with
  | Text_live (node, _) -> visit (Dom_api.to_node node)
  | Empty_live -> ()
  | List_live items | Keyed_live (_, items) -> Array.iter (each_node visit) items
  | Element_live element -> visit (Dom_api.to_node element.root)
  | Lazy_live (_, inner) -> each_node visit inner
  | Component_live instance -> (
      each_node visit instance.output;
      match instance.region with
      | Marker node -> visit (Dom_api.to_node node)
      | Nodes -> ())
  | Portal_live _ -> ()
  | Memo_live (_, inner) -> each_node visit inner

let rec front live after =
  match live with
  | Text_live (node, _) -> Some (Dom_api.to_node node)
  | Empty_live -> after
  | List_live items | Keyed_live (_, items) -> Array.fold_right front items after
  | Element_live element -> Some (Dom_api.to_node element.root)
  | Lazy_live (_, inner) -> front inner after
  | Component_live instance ->
      front instance.output
        (match instance.region with
        | Marker node -> Some (Dom_api.to_node node)
        | Nodes -> after)
  | Portal_live _ -> after
  | Memo_live (_, inner) -> front inner after

let remove (position : Template.position) live =
  let rec release live =
    match live with
    | Text_live _ | Empty_live -> ()
    | List_live items | Keyed_live (_, items) -> Array.iter release items
    | Element_live element ->
        Array.iter
          (fun binding ->
            match binding with
            | Child_site site -> release site.live
            | Handler_site handler -> Events.release handler
            | Attr_site _ | Prop_site _ | Text_site _ | Ref_site _
            | Style_site _ | Class_site _ | Unbound_site _ ->
                ())
          element.bindings
    | Lazy_live (_, inner) -> release inner
    | Component_live instance ->
        release instance.output;
        instance.status <- Unmounted;
        List.iter (fun cleanup -> cleanup ()) (List.rev instance.teardown)
    | Portal_live (container, inner) ->
        each_node (Dom_api.remove_child (Dom_api.to_node container)) inner;
        release inner
    | Memo_live (_, inner) -> release inner
  in
  each_node (Dom_api.remove_child position.parent) live;
  release live

let backwards count (position : Template.position) make =
  let result = Array.make count Empty_live in
  let rec fill i before =
    if i >= 0 then (
      let live = make i { position with before } in
      result.(i) <- live;
      fill (i - 1) (front live before))
  in
  fill (count - 1) position.before;
  result

let mark_region (position : Template.position) instance =
  match (front instance.output None, instance.region) with
  | Some _, Nodes | None, Marker _ -> ()
  | Some _, Marker node ->
      Dom_api.remove_child position.parent node;
      instance.region <- Nodes
  | None, Nodes ->
      let node = Dom_api.create_text_node Dom_api.document "" in
      insert position node;
      instance.region <- Marker node

let mismatch () = Js.Exn.raiseError "melhole: hole value does not match its template hole"

let write_attr binding value =
  match binding with
  | Attr_site site ->
      if not (Option.equal Dom_api.same site.value value) then (
        (match value with
        | Some value -> Dom_api.set_attribute site.element site.name value
        | None -> Dom_api.remove_attribute site.element site.name);
        site.value <- value)
  | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let write_prop binding value =
  match binding with
  | Prop_site site ->
      let (Input old) = site.value in
      if not (Dom_api.same old value) then (
        Dom_api.set_property site.element site.name value;
        site.value <- Input value)
  | Attr_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let write_text binding value =
  match binding with
  | Text_site site ->
      if not (Dom_api.same site.value value) then (
        Dom_api.set_data site.node value;
        site.value <- value)
  | Attr_site _ | Prop_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let write_handler binding next =
  match binding with
  | Handler_site handler -> Events.set handler next
  | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Ref_site _ | Style_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let write_ref binding callback =
  match binding with
  | Ref_site site ->
      if not (Dom_api.same site.callback callback) then (
        site.callback <- callback;
        callback site.element)
  | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Style_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let write_style binding value =
  match binding with
  | Style_site site ->
      if
        not
          (List.equal
             (fun (name, value) (name', value') ->
               Dom_api.same name name' && Dom_api.same value value')
             site.value value)
      then (
        restyle site.element site.value value;
        site.value <- value)
  | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _
  | Class_site _ | Unbound_site _ ->
      mismatch ()

let rec build position child =
  match child with
  | Text value ->
      let node = Dom_api.create_text_node Dom_api.document value in
      insert position node;
      Text_live (node, value)
  | Empty -> Empty_live
  | List children -> List_live (Array.of_list (List.map (build position) children))
  | Keyed entries ->
      Keyed_live
        ( Array.of_list (List.map fst entries),
          Array.of_list (List.map (fun (_, child) -> build position child) entries)
        )
  | Key (_, child) -> build position child
  | Element (template, holes) ->
      let root, sites = Template.clone template in
      if Array.length sites <> Array.length holes then
        Js.Exn.raiseError
          ("melhole: template expects "
          ^ string_of_int (Array.length sites)
          ^ " hole values, got "
          ^ string_of_int (Array.length holes));
      attach position { template; root; bindings = Array.map2 bind sites holes; deps = [||] }
  | Guarded (template, count, refresh) ->
      let root, sites = Template.clone template in
      let block =
        {
          template;
          root;
          bindings = Array.map (fun site -> Unbound_site site) sites;
          deps = Array.make count unset;
        }
      in
      refresh block;
      attach position block
  | Lazy thunk -> Lazy_live (thunk, build position (force thunk))
  | Component (component, inputs, render) ->
      mount_instance position (Renders { component; inputs; render })
  | Provide (context, value, children) ->
      mount_instance position (Provides { context; value; children; consumers = [] })
  | Portal (container, child) ->
      Portal_live
        ( container,
          build { Template.parent = Dom_api.to_node container; before = None } child )
  | Memo (inputs, render) -> Memo_live (inputs, build position (render ()))

and attach position block =
  Array.iter
    (fun binding ->
      match binding with
      | Unbound_site _ -> Js.Exn.raiseError "melhole: a compiled template left a hole unfilled"
      | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _
      | Style_site _ | Class_site _ ->
          ())
    block.bindings;
  insert position block.root;
  Array.iter
    (fun binding ->
      match binding with
      | Ref_site site -> site.callback site.element
      | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          ())
    block.bindings;
  Element_live block

and mount_instance position kind =
  let instance =
    {
      parent = !owner;
      depth = Option.fold ~none:0 ~some:(fun parent -> parent.depth + 1) !owner;
      slots = Hook.slots ();
      kind;
      status = Clean;
      output = Empty_live;
      region = Nodes;
      pending = [];
      teardown = [];
    }
  in
  let child = run instance in
  instance.output <- within owner (Some instance) (fun () -> build position child);
  mark_region position instance;
  commit instance;
  Component_live instance

and refresh (position : Template.position) instance =
  let child = run instance in
  let at =
    match instance.region with
    | Marker node -> { position with before = Some (Dom_api.to_node node) }
    | Nodes -> position
  in
  instance.output <- within owner (Some instance) (fun () -> update at instance.output child);
  mark_region position instance;
  commit instance

and bind site hole =
  match (site, hole) with
  | Template.Attr (element, name), HAttr value ->
      Option.iter (Dom_api.set_attribute element name) value;
      Attr_site { element; name; value }
  | Prop (element, name), HProp value ->
      Dom_api.set_property element name value;
      Prop_site { element; name; value = Input value }
  | Text position, HText value ->
      let node = Dom_api.create_text_node Dom_api.document value in
      insert position node;
      Text_site { node; value }
  | Child position, HChild child ->
      Child_site { position; source = child; live = build position child }
  | Handler (element, event), HHandler handler ->
      Handler_site (Events.bind element event handler)
  | Ref element, HRef callback -> Ref_site { element; callback }
  | Style element, HStyle value ->
      restyle element [] value;
      Style_site { element; value }
  | Class_list element, HClass value ->
      reclass element [] value;
      Class_site { element; value }
  | ( ( Attr _ | Prop _ | Text _ | Child _ | Handler _ | Ref _ | Style _
      | Class_list _ ),
      _ ) ->
      Js.Exn.raiseError "melhole: hole value does not match its template hole"

and replace position live child =
  let replacement = build position child in
  remove position live;
  replacement

and update position live child =
  match (live, child) with
  | _, Key (_, child) -> update position live child
  | Text_live (node, value), Text next ->
      if Dom_api.same value next then live
      else (
        Dom_api.set_data node next;
        Text_live (node, next))
  | Empty_live, Empty -> live
  | List_live items, List children ->
      let children = Array.of_list children in
      let old_count = Array.length items in
      for i = Array.length children to old_count - 1 do
        remove position items.(i)
      done;
      List_live
        (backwards (Array.length children) position (fun i at ->
             if i < old_count then update at items.(i) children.(i)
             else build at children.(i)))
  | Keyed_live (keys, items), Keyed entries ->
      let next_keys = Array.of_list (List.map fst entries) in
      let children = Array.of_list (List.map snd entries) in
      let plan = Reconcile.plan ~old:keys ~next:next_keys in
      List.iter (fun i -> remove position items.(i)) plan.dropped;
      Keyed_live
        ( next_keys,
          backwards (Array.length children) position (fun j at ->
              match plan.steps.(j) with
              | Create -> build at children.(j)
              | Keep i -> update at items.(i) children.(j)
              | Move i ->
                  let live = update at items.(i) children.(j) in
                  each_node (insert at) live;
                  live) )
  | Element_live element, Element (template, holes)
    when Dom_api.same element.template template ->
      Array.iter2 rewrite element.bindings holes;
      live
  | Element_live block, Guarded (template, _, refresh) when Dom_api.same block.template template ->
      refresh block;
      live
  | Lazy_live (old, inner), Lazy next -> (
      match (old, next) with
      | Lazy1 (f, a), Lazy1 (g, b) when Dom_api.same f g && Dom_api.same a b -> live
      | Lazy2 (f, a, b), Lazy2 (g, c, d)
        when Dom_api.same f g && Dom_api.same a c && Dom_api.same b d ->
          live
      | (Lazy1 _ | Lazy2 _), _ -> Lazy_live (next, update position inner (force next)))
  | Component_live instance, Component (component, inputs, render) -> (
      match instance.kind with
      | Renders definition when definition.component == component ->
          (match
             ( instance.status,
               Array.length definition.inputs = Array.length inputs
               && Array.for_all2
                    (fun (Input old) (Input next) -> Dom_api.same old next)
                    definition.inputs inputs )
           with
          | Clean, true -> ()
          | (Clean | Dirty | Unmounted), _ ->
              definition.inputs <- inputs;
              definition.render <- render;
              refresh position instance);
          live
      | Renders _ | Provides _ -> replace position live child)
  | Component_live instance, Provide (context, value, children) -> (
      match instance.kind with
      | Provides provision -> (
          match Type.Id.provably_equal provision.context.witness context.witness with
          | Some Equal ->
              if not (Dom_api.same provision.value value) then (
                provision.value <- value;
                List.iter mark_dirty provision.consumers;
                provision.consumers <- []);
              provision.children <- children;
              refresh position instance;
              live
          | None -> replace position live child)
      | Renders _ -> replace position live child)
  | Portal_live (container, inner), Portal (target, child) when Dom_api.same container target ->
      Portal_live
        ( container,
          update { Template.parent = Dom_api.to_node container; before = None } inner child )
  | Memo_live (old, inner), Memo (inputs, render) ->
      if
        Array.length old = Array.length inputs
        && Array.for_all2 (fun (Input a) (Input b) -> Dom_api.same a b) old inputs
      then live
      else Memo_live (inputs, update position inner (render ()))
  | ( ( Text_live _ | Empty_live | List_live _ | Keyed_live _ | Element_live _
      | Lazy_live _ | Component_live _ | Portal_live _ | Memo_live _ ),
      _ ) ->
      replace position live child

and rewrite binding hole =
  match (binding, hole) with
  | Attr_site _, HAttr value -> write_attr binding value
  | Prop_site _, HProp value -> write_prop binding value
  | Text_site _, HText value -> write_text binding value
  | Child_site site, HChild child ->
      if not (Dom_api.same site.source child) then (
        site.live <- update site.position site.live child;
        site.source <- child)
  | Handler_site _, HHandler next -> write_handler binding next
  | Ref_site _, HRef callback -> write_ref binding callback
  | Style_site _, HStyle value -> write_style binding value
  | Class_site site, HClass value ->
      if not (List.equal Dom_api.same site.value value) then (
        reclass site.element site.value value;
        site.value <- value)
  | ( ( Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _
      | Ref_site _ | Style_site _ | Class_site _ | Unbound_site _ ),
      _ ) ->
      mismatch ()

let put block index hole write =
  match block.bindings.(index) with
  | Unbound_site site -> block.bindings.(index) <- bind site (hole ())
  | ( Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _
    | Style_site _ | Class_site _ ) as binding ->
      write binding

let put_attr block index value = put block index (fun () -> HAttr value) (fun binding -> write_attr binding value)
let put_prop block index value = put block index (fun () -> HProp value) (fun binding -> write_prop binding value)
let put_text block index value = put block index (fun () -> HText value) (fun binding -> write_text binding value)
let put_handler block index next = put block index (fun () -> HHandler next) (fun binding -> write_handler binding next)
let put_ref block index callback = put block index (fun () -> HRef callback) (fun binding -> write_ref binding callback)
let put_style block index value = put block index (fun () -> HStyle value) (fun binding -> write_style binding value)
let put_child block index child = put block index (fun () -> HChild child) (fun binding -> rewrite binding (HChild child))

let check block index value =
  let (Input remembered) = block.deps.(index) in
  if Dom_api.same remembered value then Unchanged
  else begin
    block.deps.(index) <- Input value;
    Changed
  end

let run_passive () =
  passive_schedule := Idle;
  run_phases passive_cleanups passive_runs

let settle () =
  let position_of instance =
    let rec back live before =
      match live with
      | Text_live (node, _) -> Some (Dom_api.to_node node)
      | Empty_live -> before
      | List_live items | Keyed_live (_, items) ->
          Array.fold_left (fun found item -> back item found) before items
      | Element_live element -> Some (Dom_api.to_node element.root)
      | Lazy_live (_, inner) -> back inner before
      | Component_live inner -> (
          match inner.region with
          | Marker node -> Some (Dom_api.to_node node)
          | Nodes -> back inner.output before)
      | Portal_live _ -> before
      | Memo_live (_, inner) -> back inner before
    in
    match
      Option.bind (front (Component_live instance) None) Dom_api.parent_node,
      back (Component_live instance) None
    with
    | Some parent, Some last -> { Template.parent; before = Dom_api.next_sibling last }
    | None, _ | _, None -> Js.Exn.raiseError "melhole: a mounted component is not in the DOM"
  in
  run_layout ();
  let rec drain () =
    match !dirty with
    | [] -> ()
    | pending ->
        dirty := [];
        List.iter
          (fun instance ->
            match instance.status with
            | Dirty -> refresh (position_of instance) instance
            | Clean | Unmounted -> ())
          (List.stable_sort
             (fun a b ->
               match (front (Component_live a) None, front (Component_live b) None) with
               | Some first, Some second when not (Dom_api.same first second) ->
                   if Dom_api.compare_document_position first second land 4 <> 0 then -1 else 1
               | Some _, Some _ | None, _ | _, None -> Int.compare a.depth b.depth)
             pending);
        run_layout ();
        drain ()
  in
  drain ();
  match (!passive_cleanups, !passive_runs, !passive_schedule) with
  | [], [], _ | _, _, Queued -> ()
  | _ :: _, _, Idle | _, _ :: _, Idle ->
      passive_schedule := Queued;
      Dom_api.request_animation_frame (fun _ -> Dom_api.set_timeout run_passive 0)

let mount child ~parent ~before =
  let position =
    { Template.parent = Dom_api.to_node parent; before = Option.map Dom_api.to_node before }
  in
  let mounted = { position; source = child; live = build position child } in
  settle ();
  mounted

let patch mounted child =
  if not (Dom_api.same mounted.source child) then (
    mounted.live <- update mounted.position mounted.live child;
    mounted.source <- child;
    settle ())

let unmount mounted =
  remove mounted.position mounted.live;
  mounted.live <- Empty_live;
  mounted.source <- Empty

let current () =
  match !rendering with
  | Some instance -> instance
  | None -> Js.Exn.raiseError "melhole: hooks can only be called while a component renders"

let slot instance key init = Hook.take instance.slots key init

let invalidate instance =
  match !rendering with
  | Some _ ->
      Js.Exn.raiseError "melhole: a state setter was called while a component was rendering"
  | None -> (
      mark_dirty instance;
      match !render_schedule with
      | Queued -> ()
      | Idle ->
          render_schedule := Queued;
          Dom_api.queue_microtask (fun () ->
              render_schedule := Idle;
              settle ()))

let after_commit instance timing phase job =
  instance.pending <- (timing, phase, job) :: instance.pending
let on_unmount instance cleanup = instance.teardown <- cleanup :: instance.teardown

let consume : type value. instance -> value Context.t -> value =
 fun instance context ->
  let rec find ancestor =
    match ancestor with
    | None -> context.default
    | Some above -> (
        match above.kind with
        | Provides provision -> (
            match Type.Id.provably_equal provision.context.witness context.witness with
            | Some Equal ->
                if not (List.memq instance provision.consumers) then
                  provision.consumers <- instance :: provision.consumers;
                provision.value
            | None -> find above.parent)
        | Renders _ -> find above.parent)
  in
  find instance.parent

let flush () =
  render_schedule := Idle;
  settle ();
  run_passive ()
