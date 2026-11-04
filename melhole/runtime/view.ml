(** When an effect runs after its instance is patched. *)
type timing =
  | Layout  (** Synchronously, before the browser paints. *)
  | Passive  (** After the whole flush, once the browser has painted. *)

(** Within one commit every cleanup runs before any effect, as in React. *)
type phase = Cleanup | Run

(** A value of any type, compared physically. *)
type input = Input : 'a -> input [@@unboxed]

(** Whether a value differs from the one remembered for it. *)
type change = Changed | Unchanged

(** What a mounted child is, so that the next child can tell whether it can patch it. *)
type state = ..

(** A mounted child: its own nodes and how to let go of what they hold. *)
type live = {
  state : state;
  each : (Dom.node -> unit) -> unit;  (** Visits the top-level nodes in document order. *)
  front : Dom.node option -> Dom.node option;  (** The first node, or the given one. *)
  release : unit -> unit;  (** Releases handlers, effects and portals; the nodes stay. *)
}

(** How a child takes part in [list] and [items]. *)
type shape =
  | Single
  | Nothing
  | Entry of string * child  (** A keyed child. *)
  | Many of child list
  | Entries of (string * child) list
  | Deferred of (unit -> child)  (** Rendered on demand. *)

(** A description that mounts itself and patches a live child of its own kind. *)
and child = {
  build : Template.position -> live;
  patch : Template.position -> live -> live option;  (** [None] for another kind of child. *)
  shape : shape;
}

type hole =
  | HText of string
  | HAttr of string option
  | HProp : 'a -> hole
  | HClass of string list
  | HStyle of (string * string) list
  | HHandler of (Dom.event -> unit)
  | HRef of (Dom.element -> unit)
  | HChild of child

(** Whether a template's root is in the document yet. *)
type placement = Detached | Placed

(** A mounted template: the values its refresh last read, what to release, which refs to call. *)
type scope = {
  deps : input array;
  mutable cleanups : (unit -> unit) list;  (** Newest first. *)
  mutable references : (unit -> unit) list;  (** Called once the root is placed. *)
  mutable placement : placement;
}

(** A compiled template: its markup and how to make its holes on a fresh clone. *)
type 'holes template = {
  markup : Template.t;
  holes : scope -> Dom.node -> 'holes;
  mutable lent : 'holes option;  (** The holes of a mounted clone, handed over to one refresh. *)
}

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

(** The component scheduler, installed by the first component that mounts. *)
type scheduler = Absent | Installed of { settle : unit -> unit; flush : unit -> unit }

type state +=
  | Empty_state
  | Text_state of { node : Dom.text; mutable value : string }
  | List_state of live array
  | Keyed_state of string array * live array
  | Memo_state of input array * live
  | Portal_state of Dom.element * live
  | Compiled_state of { identity : input; scope : scope; lend : unit -> unit }
  | Static_state of Template.t

let scheduler = ref Absent

let nothing =
  { state = Empty_state; each = (fun _ -> ()); front = (fun after -> after); release = ignore }

let insert (position : Template.position) node =
  Dom_api.insert_before position.parent node (Js.Null.fromOption position.before)

let remove (position : Template.position) live =
  live.each (Dom_api.remove_child position.parent);
  live.release ()

let update position live child =
  match child.patch position live with
  | Some next -> next
  | None ->
      let next = child.build position in
      remove position live;
      next

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
    each = (fun visit -> Array.iter (fun item -> item.each visit) items);
    front = (fun after -> Array.fold_right (fun item after -> item.front after) items after);
    release = (fun () -> Array.iter (fun item -> item.release ()) items);
  }

let backwards count (position : Template.position) make =
  let result = Array.make count nothing in
  let rec fill i before =
    if i >= 0 then (
      let live = make i { position with before } in
      result.(i) <- live;
      fill (i - 1) (live.front before))
  in
  fill (count - 1) position.before;
  result

let text value =
  {
    shape = Single;
    build =
      (fun position ->
        let node = Dom_api.create_text_node Dom_api.document value in
        insert position node;
        {
          state = Text_state { node; value };
          each = (fun visit -> visit (Dom_api.to_node node));
          front = (fun _ -> Some (Dom_api.to_node node));
          release = ignore;
        });
    patch =
      (fun _ live ->
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
    build = (fun _ -> nothing);
    patch = (fun _ live -> match live.state with Empty_state -> Some live | _ -> None);
  }

let many children =
  {
    shape = Many children;
    build =
      (fun position ->
        let items = Array.of_list (List.map (fun child -> child.build position) children) in
        group (List_state items) items);
    patch =
      (fun position live ->
        match live.state with
        | List_state items ->
            let children = Array.of_list children in
            let old_count = Array.length items in
            for i = Array.length children to old_count - 1 do
              remove position items.(i)
            done;
            let items =
              backwards (Array.length children) position (fun i at ->
                  if i < old_count then update at items.(i) children.(i)
                  else children.(i).build at)
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
      (fun position ->
        let items = Array.map (fun child -> child.build position) children in
        group (Keyed_state (keys, items)) items);
    patch =
      (fun position live ->
        match live.state with
        | Keyed_state (old_keys, items) ->
            let plan = Reconcile.plan ~old:old_keys ~next:keys in
            List.iter (fun i -> remove position items.(i)) plan.dropped;
            let items =
              backwards (Array.length children) position (fun j at ->
                  match plan.steps.(j) with
                  | Create -> children.(j).build at
                  | Keep i -> update at items.(i) children.(j)
                  | Move i ->
                      let live = update at items.(i) children.(j) in
                      live.each (insert at);
                      live)
            in
            Some (group (Keyed_state (keys, items)) items)
        | _ -> None);
  }

let with_key key child = { child with shape = Entry (key, child) }

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
    build = (fun position -> skipping ((render ()).build position));
    patch =
      (fun position live ->
        match live.state with
        | Memo_state (old, inner) -> (
            match compare_inputs old inputs with
            | Unchanged -> Some live
            | Changed -> Some (skipping (update position inner (render ()))))
        | _ -> None);
  }

let lazy1 render a = memo [| Input render; Input a |] (fun () -> render a)
let lazy2 render a b = memo [| Input render; Input a; Input b |] (fun () -> render a b)

let portal container child =
  let inside = { Template.parent = Dom_api.to_node container; before = None } in
  let held inner =
    {
      state = Portal_state (container, inner);
      each = (fun _ -> ());
      front = (fun after -> after);
      release =
        (fun () ->
          inner.each (Dom_api.remove_child (Dom_api.to_node container));
          inner.release ());
    }
  in
  {
    shape = Single;
    build = (fun _ -> held (child.build inside));
    patch =
      (fun _ live ->
        match live.state with
        | Portal_state (target, inner) when Dom_api.same target container ->
            Some (held (update inside inner child))
        | _ -> None);
  }

let place position root scope state =
  insert position root;
  scope.placement <- Placed;
  List.iter (fun reference -> reference ()) (List.rev scope.references);
  {
    state;
    each = (fun visit -> visit (Dom_api.to_node root));
    front = (fun _ -> Some (Dom_api.to_node root));
    release = (fun () -> List.iter (fun cleanup -> cleanup ()) (List.rev scope.cleanups));
  }

let fresh count = { deps = Array.make count (Input (ref ())); cleanups = []; references = []; placement = Detached }

let template markup holes = { markup; holes; lent = None }

let compiled template count refresh =
  {
    shape = Single;
    build =
      (fun position ->
        let root = Template.clone template.markup in
        let scope = fresh count in
        let holes = template.holes scope (Dom_api.to_node root) in
        refresh scope holes;
        place position root scope
          (Compiled_state
             { identity = Input template; scope; lend = (fun () -> template.lent <- Some holes) }));
    patch =
      (fun _ live ->
        match live.state with
        | Compiled_state mounted ->
            let (Input identity) = mounted.identity in
            if Dom_api.same identity template then (
              mounted.lend ();
              Option.iter
                (fun holes ->
                  template.lent <- None;
                  refresh mounted.scope holes)
                template.lent;
              Some live)
            else None
        | _ -> None);
  }

let static markup =
  {
    shape = Single;
    build = (fun position -> place position (Template.clone markup) (fresh 0) (Static_state markup));
    patch =
      (fun _ live ->
        match live.state with
        | Static_state shown when Dom_api.same shown markup -> Some live
        | _ -> None);
  }

let check scope index value =
  let (Input remembered) = scope.deps.(index) in
  if Dom_api.same remembered value then Unchanged
  else (
    scope.deps.(index) <- Input value;
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
  scope.cleanups <- (fun () -> Events.release binding) :: scope.cleanups;
  ({ binding } : Hole.handler)

let set_handler (hole : Hole.handler) next = Events.set hole.binding next

let reference_hole scope element =
  let hole : Hole.reference = { scope; element; callback = ignore } in
  scope.references <- (fun () -> hole.callback hole.element) :: scope.references;
  hole

let set_reference (hole : Hole.reference) callback =
  if not (Dom_api.same hole.callback callback) then (
    hole.callback <- callback;
    match hole.scope.placement with Placed -> callback hole.element | Detached -> ())

let content_hole scope position =
  let hole : Hole.content = { position; source = empty; live = nothing } in
  scope.cleanups <- (fun () -> hole.live.release ()) :: scope.cleanups;
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

type state += Paths_state of Template.t * site array

let tpl_instance markup values =
  let mismatch () = Js.Exn.raiseError "melhole: hole value does not match its template hole" in
  let refresh sites =
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
      (fun position ->
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
        refresh sites;
        place position root scope (Paths_state (markup, sites)));
    patch =
      (fun _ live ->
        match live.state with
        | Paths_state (shown, sites) when Dom_api.same shown markup ->
            refresh sites;
            Some live
        | _ -> None);
  }

let mount child ~parent ~before =
  let position =
    { Template.parent = Dom_api.to_node parent; before = Option.map Dom_api.to_node before }
  in
  let mounted = { position; source = child; live = child.build position } in
  (match !scheduler with Absent -> () | Installed components -> components.settle ());
  mounted

let patch mounted child =
  if not (Dom_api.same mounted.source child) then (
    mounted.live <- update mounted.position mounted.live child;
    mounted.source <- child;
    match !scheduler with Absent -> () | Installed components -> components.settle ())

let unmount mounted =
  remove mounted.position mounted.live;
  mounted.live <- nothing;
  mounted.source <- empty

let flush () = match !scheduler with Absent -> () | Installed components -> components.flush ()

(** A mounted component: its hook slots, its output and its effects. *)
type instance = {
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

(** Whether a flush is already queued. *)
type schedule = Idle | Queued

type state += Instance_state of instance

let rendering = ref None
let owner = ref None
let dirty = ref []
let layout_cleanups = ref []
let layout_runs = ref []
let passive_cleanups = ref []
let passive_runs = ref []
let render_schedule = ref Idle
let passive_schedule = ref Idle

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

let mark_dirty instance =
  match instance.status with
  | Clean ->
      instance.status <- Dirty;
      dirty := instance :: !dirty
  | Dirty | Unmounted -> ()

let run_phases cleanups runs =
  let run_jobs jobs =
    List.iter
      (fun (instance, job) ->
        match instance.status with Unmounted -> () | Clean | Dirty -> job ())
      (List.rev jobs)
  in
  let pending_cleanups = !cleanups and pending_runs = !runs in
  cleanups := [];
  runs := [];
  run_jobs pending_cleanups;
  run_jobs pending_runs

let run_passive () =
  passive_schedule := Idle;
  run_phases passive_cleanups passive_runs

let instance_live instance =
  {
    state = Instance_state instance;
    each =
      (fun visit ->
        instance.output.each visit;
        match instance.region with
        | Marker node -> visit (Dom_api.to_node node)
        | Nodes -> ());
    front =
      (fun after ->
        instance.output.front
          (match instance.region with
          | Marker node -> Some (Dom_api.to_node node)
          | Nodes -> after));
    release =
      (fun () ->
        instance.output.release ();
        instance.status <- Unmounted;
        List.iter (fun cleanup -> cleanup ()) (List.rev instance.teardown));
  }

let render instance =
  instance.status <- Clean;
  match instance.kind with
  | Renders definition ->
      Hook.rewind instance.slots;
      within rendering (Some instance) definition.render
  | Provides provision -> provision.children

let commit (position : Template.position) instance =
  (match (instance.output.front None, instance.region) with
  | Some _, Nodes | None, Marker _ -> ()
  | Some _, Marker node ->
      Dom_api.remove_child position.parent node;
      instance.region <- Nodes
  | None, Nodes ->
      let node = Dom_api.create_text_node Dom_api.document "" in
      insert position node;
      instance.region <- Marker node);
  List.iter
    (fun (timing, phase, job) ->
      match (timing, phase) with
      | Layout, Cleanup -> layout_cleanups := (instance, job) :: !layout_cleanups
      | Layout, Run -> layout_runs := (instance, job) :: !layout_runs
      | Passive, Cleanup -> passive_cleanups := (instance, job) :: !passive_cleanups
      | Passive, Run -> passive_runs := (instance, job) :: !passive_runs)
    (List.rev instance.pending);
  instance.pending <- []

let refresh (position : Template.position) instance =
  let child = render instance in
  let at =
    match instance.region with
    | Marker node -> { position with before = Some (Dom_api.to_node node) }
    | Nodes -> position
  in
  instance.output <- within owner (Some instance) (fun () -> update at instance.output child);
  commit position instance

let settle () =
  let position_of instance =
    let live = instance_live instance in
    let last = ref None in
    live.each (fun node -> last := Some node);
    match (Option.bind (live.front None) Dom_api.parent_node, !last) with
    | Some parent, Some last -> { Template.parent; before = Dom_api.next_sibling last }
    | None, _ | _, None -> Js.Exn.raiseError "melhole: a mounted component is not in the DOM"
  in
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
               match ((instance_live a).front None, (instance_live b).front None) with
               | Some first, Some second when not (Dom_api.same first second) ->
                   if Dom_api.compare_document_position first second land 4 <> 0 then -1 else 1
               | Some _, Some _ | None, _ | _, None -> Int.compare a.depth b.depth)
             pending);
        run_phases layout_cleanups layout_runs;
        drain ()
  in
  run_phases layout_cleanups layout_runs;
  drain ();
  match (!passive_cleanups, !passive_runs, !passive_schedule) with
  | [], [], _ | _, _, Queued -> ()
  | _ :: _, _, Idle | _, _ :: _, Idle ->
      passive_schedule := Queued;
      Dom_api.request_animation_frame (fun _ -> Dom_api.set_timeout run_passive 0)

let mount_instance position kind =
  (match !scheduler with
  | Installed _ -> ()
  | Absent ->
      scheduler :=
        Installed
          {
            settle;
            flush =
              (fun () ->
                render_schedule := Idle;
                settle ();
                run_passive ());
          });
  let instance =
    {
      parent = !owner;
      depth = Option.fold ~none:0 ~some:(fun parent -> parent.depth + 1) !owner;
      slots = Hook.slots ();
      kind;
      status = Clean;
      output = nothing;
      region = Nodes;
      pending = [];
      teardown = [];
    }
  in
  let child = render instance in
  instance.output <- within owner (Some instance) (fun () -> child.build position);
  commit position instance;
  instance_live instance

let component definition inputs render =
  {
    shape = Single;
    build = (fun position -> mount_instance position (Renders { component = definition; inputs; render }));
    patch =
      (fun position live ->
        match live.state with
        | Instance_state instance -> (
            match instance.kind with
            | Renders rendered when Dom_api.same rendered.component definition ->
                (match (instance.status, compare_inputs rendered.inputs inputs) with
                | Clean, Unchanged -> ()
                | (Clean | Dirty | Unmounted), _ ->
                    rendered.inputs <- inputs;
                    rendered.render <- render;
                    refresh position instance);
                Some live
            | Renders _ | Provides _ -> None)
        | _ -> None);
  }

let provide : type value. value Context.t -> value -> child -> child =
 fun context value children ->
  {
    shape = Single;
    build =
      (fun position ->
        mount_instance position (Provides { context; value; children; consumers = [] }));
    patch =
      (fun position live ->
        match live.state with
        | Instance_state instance -> (
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
                    Some live
                | None -> None)
            | Renders _ -> None)
        | _ -> None);
  }

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
