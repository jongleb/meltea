(** A value of any type, compared physically. *)
(** One loop in the generated code; a loop patches only the rows its own site drew. *)
type loop_site = unit ref

type input = Input : 'a -> input [@@unboxed]

(** Whether a value differs from the one remembered for it. *)
type change = Changed | Unchanged

(** What a compiled template's frame needs at the moments only the runtime sees; [None] for nothing. *)
type 'frame duties = {
  placed : ('frame -> unit[@u]) option;  (** Once its root is in the document. *)
  released : ('frame -> unit[@u]) option;  (** When it goes. *)
  forgotten : ('frame -> unit[@u]) option;  (** After a render that threw halfway: every hole is written again. *)
}

(** A compiled template: its markup and how to make the frame of a fresh clone. *)
type 'frame template = {
  markup : Template.t;
  make : (Dom.node -> 'frame[@u]);
  duties : 'frame duties;
  witness : 'frame Witness.t;  (** Proves that frames made by this template have its type. *)
  adopt : 'frame adoptable;
}

(** How a compiled template takes over a server's nodes: given its frame on a first, unplaced
    clone, it makes its frame on the server's element. *)
and 'frame adoptable = Not_hydratable | Adopts of (Dom.node -> 'frame -> 'frame[@u])

(** How a library's kind of child reaches its nodes, made once per kind. *)
type 'value hosting = {
  visit : ('value -> (Dom.node -> unit[@u]) -> unit[@u]);  (** Visits the top-level nodes in document order. *)
  first : ('value -> Dom.node Js.null -> Dom.node Js.null[@u]);  (** The first node, or the given one. *)
  let_go : ('value -> unit[@u]);  (** Releases handlers, effects and portals; the nodes stay. *)
  kind : 'value Witness.t;
}

(** How a selector compares the key it chose with a row's: OCaml's [=] or [==]. *)
type equality = Structural | Physical

(** A compiled loop site that has selectors; the bits of [others] are the outer inputs that a row's
    other pieces read. *)
type selection = { others : int }

(** A projected piece of a loop's row that is one value in the rows whose key is the one [chosen]
    by the outer values, and another in the rest. *)
type 'item selector =
  | Selector : {
      equality : equality;
      chosen : 'key option;
      key : ('item -> int -> 'key[@u]);  (** A row's key, from its item and its index. *)
    }
      -> 'item selector

(** The selectors of one render of a compiled loop. *)
type 'item selectors = Unselecting | Selecting of selection * 'item selector array

(** A JavaScript [Map] from a selector's key to the rows that have it. *)
module Index : sig
  type t

  val of_rows : ('item -> int -> 'key[@u]) -> 'item array -> t
  val rows : t -> 'key -> int array option
end = struct
  type t

  external make : unit -> t = "Map" [@@mel.new]
  external get : t -> 'key -> int array Js.undefined = "get" [@@mel.send]
  external set : t -> 'key -> int array -> t = "set" [@@mel.send]
  external push : int array -> int -> int = "push" [@@mel.send]

  let of_rows key items =
    let index = make () in
    for row = 0 to Array.length items - 1 do
      let found = key (Array.unsafe_get items row) row [@u] in
      match Js.Undefined.toOption (get index found) with
      | Some rows -> ignore (push rows row)
      | None -> ignore (set index found [| row |])
    done;
    index

  let rows index key = Js.Undefined.toOption (get index key)
end

(** A loop's rows by every selector's key, made the first time a selection changes, for one site. *)
type indexes = Unindexed | Indexed of selection * Index.t array

(** A mounted child: what it is, so that the next child can tell whether it can patch it, and what
    it holds. *)
type live =
  | Empty_state
  | Text_state of { node : Dom.text; mutable value : string }
  | List_state of live array
  | Keyed_state of string array * live array
  | Memo_state of input array * live
  | One_keyed_state of string * live  (** A child with a key outside a list: another key is another child. *)
  | Pure_state of { identity : input; inputs : input array; inner : live }
      (** A function of its inputs; another identity is another kind of child. *)
  | Portal_state of Dom.element * live
  | Compiled_state : { template : 'frame template; frame : 'frame; root : Dom.element } -> live
  | Static_state of Template.t * Dom.element
  | Paths_state of { markup : Template.t; root : Dom.element; write : hole array -> unit; release : unit -> unit }
      (** Writes the next values to its holes. *)
  | Hosted : 'value hosting * 'value -> live
  | Each_state : {
      site : loop_site;  (** The loop in the code that drew the rows. *)
      source : input;  (** What [items] were made from. *)
      items : 'item array;
      keys : string array;
      outer : input array;  (** What the rendering reads besides the item. *)
      projections : input array;  (** Row after row, the parts of its rendering that read [outer]. *)
      lives : live array;
      selectors : 'item selectors;  (** Those of the last render: they chose the keys the rows show. *)
      indexes : indexes;
    }
      -> live
  | Rows_state : {
      template : 'frame template;
      root : ('frame -> Dom.element[@u]);
      source : input;
      items : 'item array;
      keys : string array;
      outer : input array;
      projections : input array;
      frames : 'frame array;
      selectors : 'item selectors;
      indexes : indexes;
    }
      -> live

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
  | HRef of (Dom.element Js.nullable -> unit)
  | HChild of child

type mounted = {
  position : Template.position;
  mutable source : child;
  mutable live : live;
}

let nothing = Empty_state

let rec visit live see =
  match live with
  | Empty_state | Portal_state _ -> ()
  | Text_state { node; _ } -> see (Dom_api.to_node node) [@u]
  | List_state lives | Keyed_state (_, lives) | Each_state { lives; _ } -> Array.iter (fun live -> visit live see) lives
  | Memo_state (_, inner) | One_keyed_state (_, inner) | Pure_state { inner; _ } -> visit inner see
  | Compiled_state { root; _ } | Static_state (_, root) | Paths_state { root; _ } -> see (Dom_api.to_node root) [@u]
  | Hosted (hosting, value) -> hosting.visit value see [@u]
  | Rows_state { frames; root; _ } -> Array.iter (fun frame -> see (Dom_api.to_node (root frame [@u])) [@u]) frames

let rec front live after =
  match live with
  | Empty_state | Portal_state _ -> after
  | Text_state { node; _ } -> Js.Null.return (Dom_api.to_node node)
  | List_state lives | Keyed_state (_, lives) | Each_state { lives; _ } ->
      let rec first index =
        match index = Array.length lives with
        | true -> after
        | false ->
            let found = front (Array.unsafe_get lives index) Js.null in
            if Dom_api.same found Js.null then first (index + 1) else found
      in
      first 0
  | Memo_state (_, inner) | One_keyed_state (_, inner) | Pure_state { inner; _ } -> front inner after
  | Compiled_state { root; _ } | Static_state (_, root) | Paths_state { root; _ } -> Js.Null.return (Dom_api.to_node root)
  | Hosted (hosting, value) -> hosting.first value after [@u]
  | Rows_state { frames; root; _ } -> (
      match Array.length frames with
      | 0 -> after
      | _ -> Js.Null.return (Dom_api.to_node (root (Array.unsafe_get frames 0) [@u])))

let rec release live =
  match live with
  | Empty_state | Text_state _ | Static_state _ -> ()
  | List_state lives | Keyed_state (_, lives) | Each_state { lives; _ } -> Array.iter release lives
  | Memo_state (_, inner) | One_keyed_state (_, inner) | Pure_state { inner; _ } -> release inner
  | Portal_state (container, inner) ->
      visit inner (fun [@u] node -> Dom_api.remove_child (Dom_api.to_node container) node);
      release inner
  | Compiled_state { template; frame; _ } -> Option.iter (fun released -> released frame [@u]) template.duties.released
  | Paths_state { release; _ } -> release ()
  | Hosted (hosting, value) -> hosting.let_go value [@u]
  | Rows_state { template; frames; _ } ->
      Option.iter (fun released -> Array.iter (fun frame -> released frame [@u]) frames) template.duties.released

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
type range = Settled | Collecting of collecting

(** The children a collecting content hole was last given. *)
and collecting = { mutable given : child }

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

type positions = { fork : (int -> int -> unit[@u]); join : (unit -> unit[@u]) }

let positions = ref { fork = (fun [@u] _ _ -> ()); join = (fun [@u] () -> ()) }

let remove (position : Template.position) live =
  visit live (fun [@u] node -> Dom_api.remove_child position.parent node);
  release live

(** Whether a live child's nodes are all the children of its parent. *)
type extent = Whole | Part

let extent (position : Template.position) first last =
  match
    ( Dom_api.same first (Template.first_or_end position.parent),
      Dom_api.same last (Dom_api.last_child position.parent) )
  with
  | true, true -> Whole
  | true, false | false, _ -> Part

let last_of live =
  let last = ref Js.null in
  visit live (fun [@u] node -> last := Js.Null.return node);
  !last

let rec last_among lives index =
  match index < 0 with
  | true -> Js.null
  | false ->
      let found = last_of (Array.unsafe_get lives index) in
      if Dom_api.same found Js.null then last_among lives (index - 1) else found

let spanned position live lives dropped =
  let count = Array.length lives in
  match Array.length dropped - count with
  | 0 when count > 0 -> extent position (front live Js.null) (last_among lives (count - 1))
  | _ -> Part

let update position live child =
  match child.patch position live [@u] with
  | Some next -> next
  | None -> (
      let first = front live Js.null in
      match Js.Null.toOption first with
      | None ->
          let next = child.build position [@u] in
          release live;
          next
      | Some _ -> (
          match extent position first (last_of live) with
          | Whole ->
              Dom_api.set_text_content position.parent "";
              release live;
              child.build position [@u]
          | Part ->
              let next = child.build position [@u] in
              remove position live;
              next))

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

let front_of = fun [@u] live after -> front live after

let after_of =
 fun [@u] live start ->
  match Js.Null.toOption (last_of live) with None -> start | Some node -> Dom_api.following_sibling node

let text value =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let node = !medium.text position (Some value) [@u] in
        Text_state { node; value });
    patch =
      (fun [@u] _ live ->
        match live with
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
    patch = (fun [@u] _ live -> match live with Empty_state -> Some live | _ -> None);
  }

let many children =
  {
    shape = Many children;
    build =
      (fun [@u] position ->
        let children = Array.of_list children in
        let items = !medium.built (Array.length children) position (fun [@u] index at -> (Array.unsafe_get children index).build at [@u]) after_of [@u] in
        List_state items);
    patch =
      (fun [@u] position live ->
        match live with
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
            Some (List_state items)
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
        Keyed_state (keys, items));
    patch =
      (fun [@u] position live ->
        match live with
        | Keyed_state (old_keys, items) ->
            let plan = Reconcile.plan ~old:old_keys ~next:keys in
            (match spanned position live items plan.dropped with
            | Whole ->
                Dom_api.set_text_content position.parent "";
                release live
            | Part -> Array.iter (fun i -> remove position (Array.unsafe_get items i)) plan.dropped);
            let items =
              backwards (Array.length children) position (fun [@u] j at ->
                  let step = Array.unsafe_get plan.steps j in
                  match (Reconcile.kept step, Reconcile.moved step) with
                  | None, None -> (Array.unsafe_get children j).build at [@u]
                  | Some i, _ -> update at (Array.unsafe_get items i) (Array.unsafe_get children j)
                  | None, Some i ->
                      let live = update at (Array.unsafe_get items i) (Array.unsafe_get children j) in
                      visit live (fun [@u] node -> move at.parent node at.before);
                      live)
                front_of
            in
            Some (Keyed_state (keys, items))
        | _ -> None);
  }

let with_key key child =
  let keep inner = One_keyed_state (key, inner) in
  {
    shape = Entry (key, child);
    build = (fun [@u] position -> keep (child.build position [@u]));
    patch =
      (fun [@u] position live ->
        match live with
        | One_keyed_state (shown, inner) when String.equal shown key -> Option.map keep (child.patch position inner [@u])
        | _ -> None);
  }

(** Where a row of a loop's next render comes from: an old row, by its index, or none. An int inside,
    so that a patch allocates nothing per row. *)
module Origin : sig
  type t

  val fresh : t
  val kept : int -> t
  val kept_from : t -> int option
end = struct
  type t = int

  let fresh = -1
  let kept from = from
  let kept_from origin = match origin < 0 with true -> None | false -> Some origin
end

let unset = Input (ref ())
let no_inputs : input array = [||]
let no_keys : string array = [||]

let piece_change flat at scratch pieces =
  let rec scan index =
    match index = pieces with
    | true -> Unchanged
    | false ->
        let (Input a) = Array.unsafe_get flat (at + index) in
        let (Input b) = Array.unsafe_get scratch index in
        if Dom_api.same a b then scan (index + 1) else Changed
  in
  scan 0

let pieces_copied source from target at pieces =
  for index = 0 to pieces - 1 do
    Array.unsafe_set target (at + index) (Array.unsafe_get source (from + index))
  done

type numbering = Items | Numbered

let selection others = { others }
let selector equality chosen key = Selector { equality; chosen; key }

external reused : 'drawn array -> 'item array = "%identity"

let arrayed (items : 'item array) = items

let reselected (type drawn item) (old_items : drawn array) (old_selectors : drawn selectors) indexes old_outer
    (items : item array) (selectors : item selectors) outer redraw =
  match (old_selectors, selectors) with
  | Selecting (drawn_selection, old_table), Selecting (current, table)
    when Dom_api.same drawn_selection current
         && Array.length old_table = Array.length table
         && Array.length old_items = Array.length items
         && Array.length old_outer = Array.length outer -> (
      let changed = ref 0 in
      for at = 0 to Array.length outer - 1 do
        let (Input before) = Array.unsafe_get old_outer at in
        let (Input next) = Array.unsafe_get outer at in
        if not (Dom_api.same before next) then changed := !changed lor (1 lsl at)
      done;
      let rec kept row =
        match row = Array.length items with
        | true -> `Kept
        | false ->
            if Dom_api.same (Array.unsafe_get old_items row) (Array.unsafe_get items row) then kept (row + 1)
            else `Replaced
      in
      let lookable equality chosen =
        match (equality, chosen) with
        | _, None | Physical, Some _ -> `Lookable
        | Structural, Some key -> (
            match Js.typeof key with "number" | "string" | "boolean" -> `Lookable | _ -> `Unlookable)
      in
      let rec usable j =
        match j = Array.length table with
        | true -> `Usable
        | false -> (
            let (Selector before) = Array.unsafe_get old_table j in
            let (Selector next) = Array.unsafe_get table j in
            match (lookable next.equality before.chosen, lookable next.equality next.chosen) with
            | `Lookable, `Lookable -> usable (j + 1)
            | `Unlookable, _ | _, `Unlookable -> `Unusable)
      in
      match (!changed land current.others, (match Dom_api.same old_items items with true -> `Kept | false -> kept 0), usable 0) with
      | 0, `Kept, `Usable ->
          let index = ref indexes in
          let built () =
            match !index with
            | Indexed (indexed, built) when Dom_api.same indexed current -> built
            | Indexed _ | Unindexed ->
                let built = Array.map (fun (Selector selector) -> Index.of_rows selector.key items) table in
                index := Indexed (current, built);
                built
          in
          for j = 0 to Array.length table - 1 do
            let (Selector before) = Array.unsafe_get old_table j in
            let (Selector next) = Array.unsafe_get table j in
            match (before.chosen, next.chosen) with
            | Some a, Some b when Dom_api.same a b -> ()
            | None, None -> ()
            | before_key, next_key ->
                let rows = Array.unsafe_get (built ()) j in
                let look key = Option.iter (Array.iter redraw) (Index.rows rows key) in
                Option.iter look before_key;
                Option.iter look next_key
          done;
          Some !index
      | _, (`Kept | `Replaced), (`Usable | `Unusable) -> None)
  | (Unselecting | Selecting _), _ -> None

let loop_site () = ref ()

let each site numbering source listed render project pieces outer selectors =
  let showing items =
  let count = Array.length items in
  let rendered index item =
    let child = render item index [@u] in
    match child.shape with
    | Entry (key, inner) -> (inner, key)
    | Single | Nothing | Many _ | Entries _ | Deferred _ -> (child, string_of_int index)
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let keys = (match count with 0 -> no_keys | _ -> Array.make count "") in
        let lives =
          !medium.built count position
            (fun [@u] index at ->
              let child, key = rendered index (Array.unsafe_get items index) in
              Array.unsafe_set keys index key;
              child.build at [@u])
            after_of [@u]
        in
        let projections = (match count * pieces with 0 -> no_inputs | size -> Array.make size unset) in
        for index = 0 to count - 1 do
          project (Array.unsafe_get items index) index projections (index * pieces) [@u]
        done;
        Each_state { site; source = Input source; items; keys; outer; projections; lives; selectors; indexes = Unindexed });
    patch =
      (fun [@u] position live ->
        match live with
        | Each_state old when not (Dom_api.same old.site site) -> None
        | Each_state old -> (
            let old_count = Array.length old.items in
            let outer_change = compare_inputs old.outer outer in
            let scratch = Array.make pieces unset in
            let ahead lives index =
              let rec find index =
                match index = count with
                | true -> position.before
                | false ->
                    let found = front (Array.unsafe_get lives index) Js.null in
                    if Dom_api.same found Js.null then find (index + 1) else found
              in
              find index
            in
            let redraw index =
              let item = Array.unsafe_get items index and at = index * pieces in
              project item index scratch 0 [@u];
              match piece_change old.projections at scratch pieces with
              | Unchanged -> ()
              | Changed ->
                  pieces_copied scratch 0 old.projections at pieces;
                  Array.unsafe_set old.lives index
                    (update
                       { position with before = ahead old.lives (index + 1) }
                       (Array.unsafe_get old.lives index)
                       (fst (rendered index item)))
            in
            match
              match outer_change with
              | Unchanged -> None
              | Changed -> reselected old.items old.selectors old.indexes old.outer items selectors outer redraw
            with
            | Some indexes ->
                Some
                  (Each_state
                     { site; source = Input source; items; keys = old.keys; outer; projections = old.projections; lives = old.lives; selectors; indexes })
            | None ->
            let in_place =
              match count = old_count with
              | false -> None
              | true ->
                  let made = ref [||] in
                  let rec check index =
                    match index = count with
                    | true -> Some !made
                    | false -> (
                        let item = Array.unsafe_get items index in
                        match Dom_api.same item (Array.unsafe_get old.items index) with
                        | true -> check (index + 1)
                        | false -> (
                            let child, key = rendered index item in
                            match String.equal key (Array.unsafe_get old.keys index) with
                            | false -> None
                            | true ->
                                (match Array.length !made with 0 -> made := Array.make count None | _ -> ());
                                Array.unsafe_set !made index (Some child);
                                check (index + 1)))
                  in
                  check 0
            in
            match in_place with
            | Some made ->
                let lives = old.lives and projections = old.projections in
                for index = count - 1 downto 0 do
                  let item = Array.unsafe_get items index and at = index * pieces in
                  let fresh = match Array.length made with 0 -> None | _ -> Array.unsafe_get made index in
                  match (fresh, outer_change) with
                  | Some child, _ ->
                      project item index projections at [@u];
                      Array.unsafe_set lives index
                        (update { position with before = ahead lives (index + 1) } (Array.unsafe_get lives index) child)
                  | None, Unchanged -> ()
                  | None, Changed -> redraw index
                done;
                let indexes = match Array.length made with 0 -> old.indexes | _ -> Unindexed in
                Some (Each_state { site; source = Input source; items; keys = old.keys; outer; projections; lives; selectors; indexes })
            | None ->
            let origins = Array.make count Origin.fresh and made = Array.make count None in
            let keys = (match count with 0 -> no_keys | _ -> Array.make count "") in
            let projections = (match count * pieces with 0 -> no_inputs | size -> Array.make size unset) in
            let shift = ref 0 in
            Array.iteri
              (fun index item ->
                (match outer_change with Unchanged -> () | Changed -> project item index scratch 0 [@u]);
                let fits from =
                  from >= 0 && from < old_count
                  && (match numbering with Items -> true | Numbered -> from = index)
                  && Dom_api.same (Array.unsafe_get old.items from) item
                  &&
                  match outer_change with
                  | Unchanged -> true
                  | Changed -> (
                      match piece_change old.projections (from * pieces) scratch pieces with
                      | Unchanged -> true
                      | Changed -> false)
                in
                let reuse from =
                  Array.unsafe_set origins index (Origin.kept from);
                  Array.unsafe_set keys index (Array.unsafe_get old.keys from);
                  pieces_copied old.projections (from * pieces) projections (index * pieces) pieces
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
                  Array.unsafe_set made index (Some child);
                  Array.unsafe_set keys index key;
                  match outer_change with
                  | Unchanged -> project item index projections (index * pieces) [@u]
                  | Changed -> pieces_copied scratch 0 projections (index * pieces) pieces))
              items;
            let described index =
              match Array.unsafe_get made index with
              | Some child -> child
              | None -> fst (rendered index (Array.unsafe_get items index))
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
                      let found = front (Array.unsafe_get lives index) Js.null in
                      if Dom_api.same found Js.null then ahead (index + 1) else found
                in
                let rec refresh index =
                  if index >= 0 then (
                    (match Origin.kept_from (Array.unsafe_get origins index) with
                    | Some from when from = index -> ()
                    | Some _ | None ->
                        Array.unsafe_set lives index
                          (update
                             { position with before = ahead (index + 1) }
                             (Array.unsafe_get old.lives index)
                             (described index)));
                    refresh (index - 1))
                in
                refresh (count - 1);
                Some (Each_state { site; source = Input source; items; keys; outer; projections; lives; selectors; indexes = Unindexed })
            | _, (`Same | `Moved) ->
                let plan = Reconcile.plan ~old:old.keys ~next:keys in
                (match spanned position live old.lives plan.dropped with
                | Whole ->
                    Dom_api.set_text_content position.parent "";
                    release live
                | Part -> Array.iter (fun i -> remove position (Array.unsafe_get old.lives i)) plan.dropped);
                let carried at i j =
                  match Origin.kept_from (Array.unsafe_get origins j) with
                  | Some from when from = i -> Array.unsafe_get old.lives i
                  | Some _ | None -> update at (Array.unsafe_get old.lives i) (described j)
                in
                let lives =
                  backwards count position (fun [@u] j at ->
                      let step = Array.unsafe_get plan.steps j in
                      match (Reconcile.kept step, Reconcile.moved step) with
                      | None, None -> (described j).build at [@u]
                      | Some i, _ -> carried at i j
                      | None, Some i ->
                          let live = carried at i j in
                          visit live (fun [@u] node -> move at.parent node at.before);
                          live)
                    front_of
                in
                Some (Each_state { site; source = Input source; items; keys; outer; projections; lives; selectors; indexes = Unindexed })))
        | _ -> None);
  }
  in
  {
    shape = Single;
    build = (fun [@u] position -> (showing (listed source)).build position [@u]);
    patch =
      (fun [@u] position live ->
        match live with
        | Each_state old when Dom_api.same old.site site -> (
            let (Input before) = old.source in
            match (Dom_api.same before source, compare_inputs old.outer outer) with
            | true, Unchanged -> Some live
            | true, Changed -> (showing (reused old.items)).patch position live [@u]
            | false, _ -> (showing (listed source)).patch position live [@u])
        | _ -> (showing (listed source)).patch position live [@u]);
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
  let skipping inner = Memo_state (inputs, inner) in
  {
    shape = Deferred render;
    build = (fun [@u] position -> skipping ((render ()).build position [@u]));
    patch =
      (fun [@u] position live ->
        match live with
        | Memo_state (old, inner) -> (
            match compare_inputs old inputs with
            | Unchanged -> Some live
            | Changed -> Some (skipping (update position inner (render ()))))
        | _ -> None);
  }

let pure identity inputs render =
  let named inner = Pure_state { identity = Input identity; inputs; inner } in
  {
    shape = Single;
    build = (fun [@u] position -> named ((render ()).build position [@u]));
    patch =
      (fun [@u] position live ->
        match live with
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
  {
    shape = Single;
    build =
      (fun [@u] _ -> Portal_state (container, !medium.aside (fun [@u] () -> child.build inside [@u]) [@u]));
    patch =
      (fun [@u] _ live ->
        match live with
        | Portal_state (target, inner) when Dom_api.same target container ->
            Some (Portal_state (container, update inside inner child))
        | _ -> None);
  }

let placed position root template frame =
  !medium.placed position root [@u];
  Option.iter (fun placed -> placed frame [@u]) template.duties.placed

let template markup make duties = { markup; make; duties; witness = Witness.make (); adopt = Not_hydratable }

let hydratable markup make duties adopt = { markup; make; duties; witness = Witness.make (); adopt = Adopts adopt }

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

let collecting = { browser with opened = (fun [@u] _ -> Collecting { given = empty }) }

let adopted template refresh (position : Template.position) =
  match template.adopt with
  | Not_hydratable -> mismatch ()
  | Adopts adopt ->
      let root = !medium.root position template.markup [@u] in
      let serving = !medium in
      medium := collecting;
      let collected = template.make (Dom_api.to_node (Template.clone template.markup)) [@u] in
      refresh collected [@u];
      medium := serving;
      let frame = adopt (Dom_api.to_node root) collected [@u] in
      refresh frame [@u];
      (root, frame)

let rows (type frame source item) (source : source) (listed : source -> item array) (template : frame template)
    (root : (frame -> Dom.element)[@u]) (refresh : (frame -> item -> unit)[@u]) (key : (item -> string)[@u]) project
    pieces outer selectors =
  let showing (items : item array) =
  let count = Array.length items in
  let mount (position : Template.position) item =
    let made, frame =
      match !medium.adoption with
      | Fresh_nodes ->
          let made = !medium.root position template.markup [@u] in
          let frame = template.make (Dom_api.to_node made) [@u] in
          refresh frame item [@u];
          (made, frame)
      | Server_nodes -> adopted template (fun [@u] frame -> refresh frame item [@u]) position
    in
    placed position made template frame;
    frame
  in
  let root_of frame = Dom_api.to_node (root frame [@u]) in
  let after = fun [@u] frame _ -> Dom_api.following_sibling (root_of frame) in
  let shown items keys projections frames indexes =
    Rows_state { template; root; source = Input source; items; keys; outer; projections; frames; selectors; indexes }
  in
  let drop (position : Template.position) frame =
    Dom_api.remove_child position.parent (root_of frame);
    Option.iter (fun released -> released frame [@u]) template.duties.released
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let keys = (match count with 0 -> no_keys | _ -> Array.make count "") in
        let frames =
          !medium.built count position
            (fun [@u] index at ->
              let item = Array.unsafe_get items index in
              Array.unsafe_set keys index (key item [@u]);
              mount at item)
            after [@u]
        in
        let projections = (match count * pieces with 0 -> no_inputs | size -> Array.make size unset) in
        for index = 0 to count - 1 do
          project (Array.unsafe_get items index) index projections (index * pieces) [@u]
        done;
        shown items keys projections frames Unindexed);
    patch =
      (fun [@u] position live ->
        match live with
        | Rows_state old -> (
            match Witness.equal old.template.witness template.witness with
            | None -> None
            | Some Equal ->
            let old_count = Array.length old.items in
            let outer_change = compare_inputs old.outer outer in
            let scratch = Array.make pieces unset in
            let drawn index = Array.unsafe_get items index in
            let refreshed frame item =
              refresh frame item [@u];
              frame
            in
            let redraw index =
              let item = Array.unsafe_get items index and at = index * pieces in
              project item index scratch 0 [@u];
              match piece_change old.projections at scratch pieces with
              | Unchanged -> ()
              | Changed ->
                  pieces_copied scratch 0 old.projections at pieces;
                  Array.unsafe_set old.frames index (refreshed (Array.unsafe_get old.frames index) item)
            in
            match
              match outer_change with
              | Unchanged -> None
              | Changed -> reselected old.items old.selectors old.indexes old.outer items selectors outer redraw
            with
            | Some indexes -> Some (shown items old.keys old.projections old.frames indexes)
            | None ->
            let rec in_place index =
              match index = count with
              | true -> `In_place
              | false -> (
                  let item = Array.unsafe_get items index in
                  match Dom_api.same item (Array.unsafe_get old.items index) with
                  | true -> in_place (index + 1)
                  | false -> (
                      match String.equal (key item [@u]) (Array.unsafe_get old.keys index) with
                      | true -> in_place (index + 1)
                      | false -> `Rearranged))
            in
            let shuffled () =
              let rec differing index found size =
                match (index = count, size) with
                | true, _ -> Some (Array.of_list (List.rev found))
                | false, 64 -> None
                | false, _ -> (
                    let item = Array.unsafe_get items index in
                    match Dom_api.same item (Array.unsafe_get old.items index) with
                    | true -> differing (index + 1) found size
                    | false -> (
                        let named = key item [@u] in
                        match String.equal named (Array.unsafe_get old.keys index) with
                        | true -> differing (index + 1) found size
                        | false -> differing (index + 1) ((index, named) :: found) (size + 1)))
              in
              Option.bind (differing 0 [] 0) (fun moved ->
                  let size = Array.length moved in
                  let sources = Array.make size (-1) in
                  let rec claim at =
                    match at = size with
                    | true -> Some (moved, Array.map (fun source -> fst (Array.unsafe_get moved source)) sources)
                    | false -> (
                        let wanted = snd (Array.unsafe_get moved at) in
                        let rec free source =
                          match source = size with
                          | true -> None
                          | false -> (
                              match
                                ( String.equal wanted (Array.unsafe_get old.keys (fst (Array.unsafe_get moved source))),
                                  Array.exists (fun claimed -> claimed = source) sources )
                              with
                              | true, false -> Some source
                              | true, true | false, _ -> free (source + 1))
                        in
                        Option.bind (free 0) (fun source ->
                            Array.unsafe_set sources at source;
                            claim (at + 1)))
                  in
                  claim 0)
            in
            let arrangement =
              match (count = old_count, outer_change) with
              | false, _ -> `Rearranged
              | true, Changed -> in_place 0
              | true, Unchanged -> (
                  match in_place 0 with
                  | `In_place -> `In_place
                  | `Rearranged -> Option.fold ~none:`Rearranged ~some:(fun found -> `Shuffled found) (shuffled ()))
            in
            match arrangement with
            | `Shuffled (moved, sources) ->
                let frames = old.frames and projections = old.projections and keys = old.keys in
                let taken = Array.map (fun source -> Array.unsafe_get frames source) sources in
                let was = Array.map (fun source -> Array.unsafe_get old.items source) sources in
                let ahead = ref 0 in
                for index = 0 to count - 1 do
                  match (!ahead < Array.length moved && fst (Array.unsafe_get moved !ahead) = index, Dom_api.same (Array.unsafe_get items index) (Array.unsafe_get old.items index)) with
                  | true, _ -> incr ahead
                  | false, true -> ()
                  | false, false ->
                      let item = Array.unsafe_get items index in
                      project item index projections (index * pieces) [@u];
                      Array.unsafe_set frames index (refreshed (Array.unsafe_get frames index) item)
                done;
                Array.iteri
                  (fun at (index, named) ->
                    let item = Array.unsafe_get items index in
                    Array.unsafe_set keys index named;
                    project item index projections (index * pieces) [@u];
                    Array.unsafe_set frames index
                      (match Dom_api.same item (Array.unsafe_get was at) with
                      | true -> Array.unsafe_get taken at
                      | false -> refreshed (Array.unsafe_get taken at) item))
                  moved;
                for at = Array.length moved - 1 downto 0 do
                  let index = fst (Array.unsafe_get moved at) in
                  let before =
                    match index + 1 = count with
                    | true -> position.before
                    | false -> Js.Null.return (root_of (Array.unsafe_get frames (index + 1)))
                  in
                  move position.parent (root_of (Array.unsafe_get frames index)) before
                done;
                Some (shown items keys projections frames Unindexed)
            | `In_place ->
                let frames = old.frames and projections = old.projections and indexes = ref old.indexes in
                for index = 0 to count - 1 do
                  let item = Array.unsafe_get items index and at = index * pieces in
                  match (Dom_api.same item (Array.unsafe_get old.items index), outer_change) with
                  | true, Unchanged -> ()
                  | true, Changed -> redraw index
                  | false, _ ->
                      indexes := Unindexed;
                      project item index projections at [@u];
                      Array.unsafe_set frames index (refreshed (Array.unsafe_get frames index) item)
                done;
                Some (shown items old.keys projections frames !indexes)
            | `Rearranged ->
            let next = Array.make count Origin.fresh and keys = (match count with 0 -> no_keys | _ -> Array.make count "") in
            let projections = (match count * pieces with 0 -> no_inputs | size -> Array.make size unset) in
            let fits from item =
              from >= 0 && from < old_count
              && Dom_api.same (Array.unsafe_get old.items from) item
              &&
              match outer_change with
              | Unchanged -> true
              | Changed -> (
                  match piece_change old.projections (from * pieces) scratch pieces with
                  | Unchanged -> true
                  | Changed -> false)
            in
            let reuse index from =
              Array.unsafe_set next index (Origin.kept from);
              Array.unsafe_set keys index (Array.unsafe_get old.keys from);
              pieces_copied old.projections (from * pieces) projections (index * pieces) pieces
            in
            let shift = ref 0 in
            for index = 0 to count - 1 do
              let item = Array.unsafe_get items index in
              (match outer_change with Unchanged -> () | Changed -> project item index scratch 0 [@u]);
              let from = index + !shift in
              if fits from item then reuse index from
              else if fits (from + 1) item then (
                incr shift;
                reuse index (from + 1))
              else if fits (from - 1) item then (
                decr shift;
                reuse index (from - 1))
              else (
                Array.unsafe_set keys index (key item [@u]);
                match outer_change with
                | Unchanged -> project item index projections (index * pieces) [@u]
                | Changed -> pieces_copied scratch 0 projections (index * pieces) pieces)
            done;
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
                let frames = Array.copy old.frames in
                for index = 0 to count - 1 do
                  match Origin.kept_from (Array.unsafe_get next index) with
                  | Some from when from = index -> ()
                  | Some _ | None -> Array.unsafe_set frames index (refreshed (Array.unsafe_get frames index) (drawn index))
                done;
                Some (shown items keys projections frames Unindexed)
            | _, (`Same | `Moved) ->
                let plan = Reconcile.plan ~old:old.keys ~next:keys in
                let whole =
                  match Array.length plan.dropped - old_count with
                  | 0 when old_count > 0 -> (
                      match
                        ( Dom_api.same
                            (Js.Null.return (root_of (Array.unsafe_get old.frames 0)))
                            (Template.first_or_end position.parent),
                          Dom_api.same
                            (Js.Null.return (root_of (Array.unsafe_get old.frames (old_count - 1))))
                            (Dom_api.last_child position.parent) )
                      with
                      | true, true -> `Whole
                      | true, false | false, _ -> `Part)
                  | _ -> `Part
                in
                (match whole with
                | `Whole ->
                    Dom_api.set_text_content position.parent "";
                    Option.iter (fun released -> Array.iter (fun frame -> released frame [@u]) old.frames) template.duties.released
                | `Part -> Array.iter (fun i -> drop position (Array.unsafe_get old.frames i)) plan.dropped);
                let carried i j =
                  match Origin.kept_from (Array.unsafe_get next j) with
                  | Some from when from = i -> Array.unsafe_get old.frames i
                  | Some _ | None -> refreshed (Array.unsafe_get old.frames i) (drawn j)
                in
                let settled j before =
                  let step = Array.unsafe_get plan.steps j in
                  match (Reconcile.kept step, Reconcile.moved step) with
                  | None, None -> mount { position with before } (drawn j)
                  | Some i, _ -> carried i j
                  | None, Some i ->
                      let moved = carried i j in
                      move position.parent (root_of moved) before;
                      moved
                in
                let frames =
                  match count with
                  | 0 -> [||]
                  | _ ->
                      let last = settled (count - 1) position.before in
                      let frames = Array.make count last in
                      let before = ref (Js.Null.return (root_of last)) in
                      for j = count - 2 downto 0 do
                        let frame = settled j !before in
                        Array.unsafe_set frames j frame;
                        before := Js.Null.return (root_of frame)
                      done;
                      frames
                in
                Some (shown items keys projections frames Unindexed)))
        | _ -> None);
  }
  in
  {
    shape = Single;
    build = (fun [@u] position -> (showing (listed source)).build position [@u]);
    patch =
      (fun [@u] position live ->
        match live with
        | Rows_state old -> (
            let (Input before) = old.source in
            match (Dom_api.same before source, Witness.equal old.template.witness template.witness) with
            | true, Some Equal -> (
                match compare_inputs old.outer outer with
                | Unchanged -> Some live
                | Changed -> (showing (reused old.items)).patch position live [@u])
            | true, None | false, _ -> (showing (listed source)).patch position live [@u])
        | _ -> (showing (listed source)).patch position live [@u]);
  }

let compiled (type frame) (template : frame template) (refresh : (frame -> unit)[@u]) =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root, frame =
          match !medium.adoption with
          | Fresh_nodes ->
              let root = !medium.root position template.markup [@u] in
              let frame = template.make (Dom_api.to_node root) [@u] in
              refresh frame [@u];
              (root, frame)
          | Server_nodes -> adopted template refresh position
        in
        placed position root template frame;
        Compiled_state { template; frame; root });
    patch =
      (fun [@u] _ live ->
        match live with
        | Compiled_state mounted -> (
            match Witness.equal mounted.template.witness template.witness with
            | Some Equal ->
                refresh mounted.frame [@u];
                Some live
            | None -> None)
        | _ -> None);
  }

let static markup =
  {
    shape = Single;
    build =
      (fun [@u] position ->
        let root = !medium.root position markup [@u] in
        !medium.placed position root [@u];
        Static_state (markup, root));
    patch =
      (fun [@u] _ live ->
        match live with
        | Static_state (shown, _) when Dom_api.same shown markup -> Some live
        | _ -> None);
  }

let rec forget live =
  match live with
  | Compiled_state { template; frame; _ } -> Option.iter (fun forgotten -> forgotten frame [@u]) template.duties.forgotten
  | One_keyed_state (_, inner) | Memo_state (_, inner) | Pure_state { inner; _ } -> forget inner
  | _ -> ()

let check (Input remembered) value = if Dom_api.same remembered value then Unchanged else Changed

(** Whether a template's root is in the document yet. *)
type placement = Detached | Placed

module Hole = struct
  type text = { node : Dom.text; mutable value : string }
  type attr = { element : Dom.element; name : string; mutable value : string option }
  (** What else a property write updates: React keeps an input's [value] attribute in step, and
      gives a checkbox the [checked] attribute it had when mounted. *)
  type echo =
    | Alone
    | Default_value
    | Default_checked_at_mount
    | Select_value  (** Set again once the whole write is done, when the select's options exist. *)

  type prop = { element : Dom.element; name : string; mutable value : input; echo : echo }
  type style = { element : Dom.element; mutable value : (string * string) list }
  type classes = { element : Dom.element; mutable value : string list }
  type handler = { element : Dom.element; event : string; key : string }
  type reference = {
    mutable placement : placement;
    element : Dom.element;
    mutable identity : input;  (** What tells this ref from another, like React's ref object or function. *)
    mutable callback : Dom.element Js.nullable -> unit;
  }
  type content = { parent : Dom.node; before : Dom.node Js.null; mutable live : live; range : range }
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

let attachments = ref []
let writing = ref 0
let attach_later attach = attachments := attach :: !attachments

let prop_hole element name : Hole.prop = { element; name; value = unset; echo = Alone }

let set_prop (hole : Hole.prop) value =
  let (Input old) = hole.value in
  if not (Dom_api.same old value) then (
    (match (!medium.adoption, hole.name, Dom_api.same hole.value unset) with
    | Server_nodes, "innerHTML", true -> ()
    | _ -> Dom_api.set_property hole.element hole.name value);
    (match hole.echo with
    | Alone -> ()
    | Default_value -> (
        Events.track hole.element;
        match (Dom_api.property hole.element "type" : string) with
        | "number" when Dom_api.same (Dom_api.active_element Dom_api.document) (Js.Null.return hole.element) -> ()
        | _ -> Dom_api.set_property hole.element "defaultValue" value)
    | Default_checked_at_mount -> (
        Events.track hole.element;
        match Dom_api.same hole.value unset with
        | true -> Dom_api.set_property hole.element "defaultChecked" value
        | false -> ())
    | Select_value ->
        attach_later (fun () ->
            let (Input chosen) = hole.value in
            if not (Dom_api.same (Dom_api.property hole.element "value") chosen) then
              Dom_api.set_property hole.element "value" chosen));
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
        | "value", "SELECT" -> Select_value
        | _ -> Alone);
    }
  in
  let restore _ =
    restores :=
      (fun () ->
        let (Input value) = hole.value in
        if not (Dom_api.same (Dom_api.property element name) value) then (
          Dom_api.set_property element name value;
          Events.track element))
      :: !restores;
    Dom_api.queue_microtask (fun () -> Dom_api.queue_microtask restore_controlled)
  in
  ignore (Events.register element "input");
  ignore (Events.register element "change");
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
    let written name value =
      match (String.length name >= 2 && String.unsafe_get name 0 = '-' && String.unsafe_get name 1 = '-', value) with
      | true, "" -> Dom_api.remove_style_property style name
      | true, _ -> Dom_api.set_style_property style name value ""
      | false, _ -> Dom_api.set_style style name value
    in
    List.iter (fun (name, _) -> written name "") hole.value;
    List.iter (fun (name, value) -> written name value) value;
    hole.value <- value)

let classes_hole element : Hole.classes = { element; value = [] }

let set_classes (hole : Hole.classes) value =
  if not (List.equal Dom_api.same hole.value value) then (
    let classes = Dom_api.class_list hole.element in
    List.iter (Dom_api.remove_class classes) hole.value;
    List.iter (Dom_api.add_class classes) value;
    (match value with [] -> Dom_api.remove_attribute hole.element "class" | _ :: _ -> ());
    hole.value <- value)

let handler_hole element event =
  Events.listen element event;
  ({ element; event; key = Events.key_of event } : Hole.handler)

let set_handler (hole : Hole.handler) next = Events.set hole.element hole.key next

let text_at position = !medium.text position None [@u]

let put_text (Input last) node value = if not (Dom_api.same last value) then Dom_api.set_data node value

let write_sole_text element value =
  match Dom_api.first_child element with
  | Some node when Dom_api.node_type node = 3 -> Dom_api.set_data (Dom_api.unsafe_text node) value
  | Some _ | None -> Dom_api.set_text_content (Dom_api.to_node element) value

let put_sole_text (Input last) element value = if not (Dom_api.same last value) then write_sole_text element value

let write_attr element name value =
  match value with
  | Some value -> Dom_api.set_attribute element name value
  | None -> Dom_api.remove_attribute element name

let put_attr (Input last) element name value = if not (Dom_api.same last value) then write_attr element name value

let event_key event = Events.key_of event

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


let attach_references () =
  let pending = !attachments in
  attachments := [];
  List.iter (fun attach -> attach ()) (List.rev pending)

let written run =
  incr writing;
  match run () with
  | result ->
      decr writing;
      (match !writing with 0 -> attach_references () | _ -> ());
      result
  | exception error ->
      decr writing;
      raise error

let reference_hole element : Hole.reference = { placement = Detached; element; identity = unset; callback = ignore }

let attached (hole : Hole.reference) () =
  match hole.placement with Placed -> hole.callback (Js.Nullable.return hole.element) | Detached -> ()

let place_reference (hole : Hole.reference) =
  hole.placement <- Placed;
  attach_later (attached hole)

let release_reference (hole : Hole.reference) =
  hole.placement <- Detached;
  hole.callback Js.Nullable.null

let set_reference (hole : Hole.reference) identity callback =
  let (Input held) = hole.identity in
  if not (Dom_api.same held identity) then (
    let previous = hole.callback in
    hole.identity <- Input identity;
    hole.callback <- callback;
    match hole.placement with
    | Placed ->
        previous Js.Nullable.null;
        attach_later (attached hole)
    | Detached -> ())

let content_hole (position : Template.position) : Hole.content =
  { parent = position.parent; before = position.before; live = nothing; range = !medium.opened position [@u] }

let release_content (hole : Hole.content) = release hole.live

let set_content (hole : Hole.content) child =
  match hole.range with
  | Settled -> hole.live <- update { parent = hole.parent; before = hole.before } hole.live child
  | Collecting collecting -> collecting.given <- child

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

let adopt_content cursor child ending : Hole.content =
  let live = child.build { Template.parent = cursor.parent; before = cursor.at } [@u] in
  cursor.at <- after_of live cursor.at [@u];
  let before = match ending with Before_next -> cursor.at | Before_anchor -> Js.Null.return (anchor cursor) in
  { parent = cursor.parent; before; live; range = Settled }

let adopt_forked cursor child ending path =
  let pairs = Array.length path / 2 in
  for at = 0 to pairs - 1 do
    !positions.fork (Array.unsafe_get path (2 * at)) (Array.unsafe_get path ((2 * at) + 1)) [@u]
  done;
  let hole = adopt_content cursor child ending in
  for _ = 1 to pairs do
    !positions.join () [@u]
  done;
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

let collected (hole : Hole.content) =
  match hole.range with Collecting collecting -> collecting.given | Settled -> mismatch ()
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
        | Reference_hole hole, HRef value -> set_reference hole value value
        | Content_hole hole, HChild value -> set_content hole value
        | ( ( Text_hole _ | Attr_hole _ | Prop_hole _ | Style_hole _ | Class_hole _
            | Handler_hole _ | Reference_hole _ | Content_hole _ ),
            _ ) ->
            mismatch ())
      sites values
  in
  let shown position root sites =
    !medium.placed position root [@u];
    Array.iter (fun site -> match site with Reference_hole hole -> place_reference hole | _ -> ()) sites;
    Paths_state
      {
        markup;
        root;
        write = refresh sites;
        release =
          (fun () ->
            Array.iter
              (fun site ->
                match site with
                | Handler_hole hole -> Events.unlisten hole.element hole.event
                | Reference_hole hole -> release_reference hole
                | Content_hole hole -> release_content hole
                | Text_hole _ | Attr_hole _ | Prop_hole _ | Style_hole _ | Class_hole _ -> ())
              sites);
      }
  in
  let adopt_instance (position : Template.position) =
    let root = !medium.root position markup [@u] in
    let paths = Template.paths markup in
    let elements = ref [] in
    let adopted = Array.make (Array.length paths) None in
    let slot cursor index ending =
      Array.unsafe_set adopted index
        (match (Array.unsafe_get paths index, Array.unsafe_get values index) with
        | Text _, HText text -> Some (Text_hole { node = adopt_text cursor text; value = text })
        | Child _, HChild child -> Some (Content_hole (adopt_content cursor child ending))
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
          | Handler (path, event) -> Handler_hole (handler_hole (at path) event)
          | Ref path -> Reference_hole (reference_hole (at path))
          | Style path -> Style_hole (style_hole (at path))
          | Class_list path -> Class_hole (classes_hole (at path)))
        paths
    in
    refresh sites values;
    shown position root sites
  in
  {
    shape = Single;
    build =
      (fun [@u] position ->
        match !medium.adoption with
        | Server_nodes -> adopt_instance position
        | Fresh_nodes ->
        let root = !medium.root position markup [@u] in
        let sites =
          Array.map
            (fun (site : (Dom.element, Template.position) Template.hole) ->
              match site with
              | Text at -> Text_hole (text_hole at)
              | Child at -> Content_hole (content_hole at)
              | Attr (element, name) -> Attr_hole (attr_hole element name)
              | Prop (element, name) -> Prop_hole (prop_hole element name)
              | Handler (element, event) -> Handler_hole (handler_hole element event)
              | Ref element -> Reference_hole (reference_hole element)
              | Style element -> Style_hole (style_hole element)
              | Class_list element -> Class_hole (classes_hole element))
            (Template.locate markup (Dom_api.to_node root))
        in
        refresh sites values;
        shown position root sites);
    patch =
      (fun [@u] _ live ->
        match live with
        | Paths_state shown when Dom_api.same shown.markup markup ->
            shown.write values;
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
  written (fun () -> { position; source = child; live = child.build position [@u] })

let patch mounted child =
  if not (Dom_api.same mounted.source child) then
    written (fun () ->
        mounted.live <- update mounted.position mounted.live child;
        mounted.source <- child)

let mount_over child ~parent =
  let parent = Dom_api.to_node parent in
  let position = { Template.parent; before = Js.null } in
  written (fun () ->
      { position; source = child; live = child.build { position with before = Dom_api.first_child_or_end parent } [@u] })

let unmount mounted =
  remove mounted.position mounted.live;
  mounted.live <- nothing;
  mounted.source <- empty

let host ~build ~patch = { shape = Single; build; patch }
let build child position = child.build position [@u]
let hosting ~visit ~first ~let_go = { visit; first; let_go; kind = Witness.make () }
let live hosting value = Hosted (hosting, value)

let hosted (type value) (hosting : value hosting) live : value option =
  match live with
  | Hosted (held, value) -> (
      match Witness.equal held.kind hosting.kind with Some Equal -> Some value | None -> None)
  | _ -> None

(** The groups of a chunked list, each a wrapper element and the keyed rows inside it. *)
type chunked = { mutable groups : (Dom.element * live) array }

let chunking =
  lazy
    (hosting
       ~visit:(fun [@u] chunked visit ->
         Array.iter (fun (wrapper, _) -> visit (Dom_api.to_node wrapper) [@u]) chunked.groups)
       ~first:(fun [@u] chunked after ->
         match chunked.groups with [||] -> after | groups -> Js.Null.return (Dom_api.to_node (fst (Array.unsafe_get groups 0))))
       ~let_go:(fun [@u] chunked -> Array.iter (fun (_, live) -> release live) chunked.groups))

let chunks ~size ~row_height ~tag child =
  let entries =
    Array.of_list
      (List.mapi
         (fun index row -> match row.shape with Entry (key, inner) -> (key, inner) | _ -> (string_of_int index, row))
         (items child))
  in
  let count = (Array.length entries + size - 1) / size in
  let group index = keyed (Array.to_list (Array.sub entries (index * size) (min size (Array.length entries - (index * size))))) in
  let wrapper () =
    let element = Dom_api.create_element Dom_api.document tag in
    Dom_api.set_attribute element "style"
      ("content-visibility:auto;contain-intrinsic-size:auto " ^ string_of_int (size * row_height) ^ "px");
    element
  in
  let made (position : Template.position) index =
    let element = wrapper () in
    insert position (Dom_api.to_node element);
    (element, build (group index) { parent = Dom_api.to_node element; before = Js.null })
  in
  host
    ~build:(fun [@u] position -> live (Lazy.force chunking) { groups = Array.init count (made position) })
    ~patch:(fun [@u] position current ->
      Option.map
        (fun chunked ->
          let old = chunked.groups in
          let kept = min count (Array.length old) in
          let slice index = Array.sub entries (index * size) (min size (Array.length entries - (index * size))) in
          let same_keys index =
            match snd (Array.unsafe_get old index) with
            | Keyed_state (keys, _) -> (
                let next = slice index in
                match Array.length keys = Array.length next with
                | true -> (
                    match Array.for_all2 (fun key (next_key, _) -> String.equal key next_key) keys next with
                    | true -> `Same
                    | false -> `Changed)
                | false -> `Changed)
            | _ -> `Changed
          in
          let sameness = Array.init kept same_keys in
          let pool = Keyed.make () and pooled = ref [] in
          Array.iteri
            (fun index (_, rows) ->
              match (index < kept && Array.unsafe_get sameness index = `Same, rows) with
              | true, _ -> ()
              | false, Keyed_state (keys, items) ->
                  Array.iteri
                    (fun at key ->
                      Keyed.set pool key (List.length !pooled);
                      pooled := (key, Array.unsafe_get items at) :: !pooled)
                    keys
              | false, _ -> ())
            old;
          let pooled = Array.of_list (List.rev !pooled) in
          let claimed = Array.make (Array.length pooled) `Free in
          let regrouped element index =
            let rows = slice index in
            let into = { Template.parent = Dom_api.to_node element; before = Js.null } in
            let items =
              Array.map
                (fun (key, child) ->
                  match Keyed.find pool key with
                  | at when at >= 0 && Array.unsafe_get claimed at = `Free ->
                      Array.unsafe_set claimed at `Taken;
                      let live = update into (snd (Array.unsafe_get pooled at)) child in
                      visit live (fun [@u] node -> move into.parent node into.before);
                      live
                  | _ -> child.build into [@u])
                rows
            in
            (element, Keyed_state (Array.map fst rows, items))
          in
          let after =
            match kept with
            | 0 -> position
            | _ -> { position with before = Js.Null.fromOption (Dom_api.next_sibling (fst (Array.unsafe_get old (kept - 1)))) }
          in
          let groups =
            Array.init count (fun index ->
                match index < kept with
                | true -> (
                    let element, rows = Array.unsafe_get old index in
                    match Array.unsafe_get sameness index with
                    | `Same -> (element, update { parent = Dom_api.to_node element; before = Js.null } rows (group index))
                    | `Changed -> regrouped element index)
                | false ->
                    let element = wrapper () in
                    insert after (Dom_api.to_node element);
                    regrouped element index)
          in
          Array.iteri
            (fun at (_, live) ->
              match Array.unsafe_get claimed at with
              | `Taken -> ()
              | `Free ->
                  visit live (fun [@u] node -> Option.iter (fun parent -> Dom_api.remove_child parent node) (Dom_api.parent_node node));
                  release live)
            pooled;
          Array.iteri
            (fun index (element, _) ->
              match index < kept with true -> () | false -> Dom_api.remove_child position.parent element)
            old;
          chunked.groups <- groups;
          current)
        (hosted (Lazy.force chunking) current))
