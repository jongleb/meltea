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

(** A mounted template: its root, its holes and the values its compiled refresh last read. *)
type block = {
  template : Template.t;
  root : Dom.element;
  bindings : binding array;
  deps : input array;
  mutable cleanups : (unit -> unit) list;  (** Newest first. *)
}

(** A template hole bound to its DOM site, with the value last written. *)
and binding =
  | Unbound_site of (Dom.element, Template.position) Template.hole
      (** Not filled yet: a compiled refresh fills every hole on mount. *)
  | Attr_site of { element : Dom.element; name : string; mutable value : string option }
  | Prop_site of { element : Dom.element; name : string; mutable value : input }
  | Text_site of { node : Dom.text; mutable value : string }
  | Child_site of child_site
  | Handler_site of Events.t
  | Ref_site of { element : Dom.element; mutable callback : Dom.element -> unit }
  | Style_site of { element : Dom.element; mutable value : (string * string) list }
  | Class_site of { element : Dom.element; mutable value : string list }

(** Child content in a hole, with the description it last showed. *)
and child_site = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
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
  | Block_state of block

let scheduler = ref Absent
let unset = Input (ref ())

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

let mismatch () = Js.Exn.raiseError "melhole: hole value does not match its template hole"

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

let guarded template count refresh =
  {
    shape = Single;
    build =
      (fun position ->
        let root, sites = Template.clone template in
        let block =
          {
            template;
            root;
            bindings = Array.map (fun site -> Unbound_site site) sites;
            deps = Array.make count unset;
            cleanups = [];
          }
        in
        refresh block;
        Array.iter
          (fun binding ->
            match binding with
            | Unbound_site _ ->
                Js.Exn.raiseError "melhole: a compiled template left a hole unfilled"
            | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _
            | Ref_site _ | Style_site _ | Class_site _ ->
                ())
          block.bindings;
        insert position block.root;
        Array.iter
          (fun binding ->
            match binding with
            | Ref_site site -> site.callback site.element
            | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _
            | Style_site _ | Class_site _ | Unbound_site _ ->
                ())
          block.bindings;
        {
          state = Block_state block;
          each = (fun visit -> visit (Dom_api.to_node block.root));
          front = (fun _ -> Some (Dom_api.to_node block.root));
          release = (fun () -> List.iter (fun cleanup -> cleanup ()) (List.rev block.cleanups));
        });
    patch =
      (fun _ live ->
        match live.state with
        | Block_state block when Dom_api.same block.template template ->
            refresh block;
            Some live
        | _ -> None);
  }

let static template = guarded template 0 ignore

let check block index value =
  let (Input remembered) = block.deps.(index) in
  if Dom_api.same remembered value then Unchanged
  else (
    block.deps.(index) <- Input value;
    Changed)

let put block index create write =
  match block.bindings.(index) with
  | Unbound_site site -> block.bindings.(index) <- create site
  | ( Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _
    | Style_site _ | Class_site _ ) as binding ->
      write binding

let put_attr block index value =
  put block index
    (fun site ->
      match site with
      | Template.Attr (element, name) ->
          Option.iter (Dom_api.set_attribute element name) value;
          Attr_site { element; name; value }
      | Prop _ | Text _ | Child _ | Handler _ | Ref _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Attr_site site ->
          if not (Option.equal Dom_api.same site.value value) then (
            (match value with
            | Some value -> Dom_api.set_attribute site.element site.name value
            | None -> Dom_api.remove_attribute site.element site.name);
            site.value <- value)
      | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_prop block index value =
  put block index
    (fun site ->
      match site with
      | Template.Prop (element, name) ->
          Dom_api.set_property element name value;
          Prop_site { element; name; value = Input value }
      | Attr _ | Text _ | Child _ | Handler _ | Ref _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Prop_site site ->
          let (Input old) = site.value in
          if not (Dom_api.same old value) then (
            Dom_api.set_property site.element site.name value;
            site.value <- Input value)
      | Attr_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_text block index value =
  put block index
    (fun site ->
      match site with
      | Template.Text position ->
          let node = Dom_api.create_text_node Dom_api.document value in
          insert position node;
          Text_site { node; value }
      | Attr _ | Prop _ | Child _ | Handler _ | Ref _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Text_site site ->
          if not (Dom_api.same site.value value) then (
            Dom_api.set_data site.node value;
            site.value <- value)
      | Attr_site _ | Prop_site _ | Child_site _ | Handler_site _ | Ref_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_handler block index next =
  put block index
    (fun site ->
      match site with
      | Template.Handler (element, event) ->
          let handler = Events.bind element event next in
          block.cleanups <- (fun () -> Events.release handler) :: block.cleanups;
          Handler_site handler
      | Attr _ | Prop _ | Text _ | Child _ | Ref _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Handler_site handler -> Events.set handler next
      | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Ref_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_ref block index callback =
  put block index
    (fun site ->
      match site with
      | Template.Ref element -> Ref_site { element; callback }
      | Attr _ | Prop _ | Text _ | Child _ | Handler _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Ref_site site ->
          if not (Dom_api.same site.callback callback) then (
            site.callback <- callback;
            callback site.element)
      | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_style block index value =
  let restyle element old =
    let style = Dom_api.style element in
    List.iter (fun (name, _) -> Dom_api.set_style style name "") old;
    List.iter (fun (name, value) -> Dom_api.set_style style name value) value;
    match value with [] -> Dom_api.remove_attribute element "style" | _ :: _ -> ()
  in
  put block index
    (fun site ->
      match site with
      | Template.Style element ->
          restyle element [];
          Style_site { element; value }
      | Attr _ | Prop _ | Text _ | Child _ | Handler _ | Ref _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Style_site site ->
          if
            not
              (List.equal
                 (fun (name, value) (name', value') ->
                   Dom_api.same name name' && Dom_api.same value value')
                 site.value value)
          then (
            restyle site.element site.value;
            site.value <- value)
      | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _ | Ref_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let put_child block index child =
  put block index
    (fun site ->
      match site with
      | Template.Child position ->
          let site : child_site = { position; source = child; live = child.build position } in
          block.cleanups <- (fun () -> site.live.release ()) :: block.cleanups;
          Child_site site
      | Attr _ | Prop _ | Text _ | Handler _ | Ref _ | Style _ | Class_list _ -> mismatch ())
    (fun binding ->
      match binding with
      | Child_site site ->
          if not (Dom_api.same site.source child) then (
            site.live <- update site.position site.live child;
            site.source <- child)
      | Attr_site _ | Prop_site _ | Text_site _ | Handler_site _ | Ref_site _ | Style_site _
      | Class_site _ | Unbound_site _ ->
          mismatch ())

let tpl_instance template holes =
  guarded template 0 (fun block ->
      if Array.length block.bindings <> Array.length holes then
        Js.Exn.raiseError
          ("melhole: template expects "
          ^ string_of_int (Array.length block.bindings)
          ^ " hole values, got "
          ^ string_of_int (Array.length holes));
      Array.iteri
        (fun index hole ->
          match hole with
          | HText value -> put_text block index value
          | HAttr value -> put_attr block index value
          | HProp value -> put_prop block index value
          | HStyle value -> put_style block index value
          | HHandler next -> put_handler block index next
          | HRef callback -> put_ref block index callback
          | HChild child -> put_child block index child
          | HClass value ->
              let reclass element old =
                let classes = Dom_api.class_list element in
                List.iter (Dom_api.remove_class classes) old;
                List.iter (Dom_api.add_class classes) value;
                match value with
                | [] -> Dom_api.remove_attribute element "class"
                | _ :: _ -> ()
              in
              put block index
                (fun site ->
                  match site with
                  | Template.Class_list element ->
                      reclass element [];
                      Class_site { element; value }
                  | Attr _ | Prop _ | Text _ | Child _ | Handler _ | Ref _ | Style _ ->
                      mismatch ())
                (fun binding ->
                  match binding with
                  | Class_site site ->
                      if not (List.equal Dom_api.same site.value value) then (
                        reclass site.element site.value;
                        site.value <- value)
                  | Attr_site _ | Prop_site _ | Text_site _ | Child_site _ | Handler_site _
                  | Ref_site _ | Style_site _ | Unbound_site _ ->
                      mismatch ()))
        holes)

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
