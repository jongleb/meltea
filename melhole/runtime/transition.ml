(** Which animation an element runs. *)
type phase = Entering | Leaving | Moving_by of float * float

(** Whether a running phase is still its element's latest. *)
type turn = Current | Stale

(** Whether a wrapper also slides the elements that stay but move. *)
type motion = Still | Sliding

(** A mounted wrapper: what it shows, and the elements still leaving. *)
type shown = { name : string; motion : motion; mutable inner : View.live; mutable leaving : Dom.element list }

type View.hosted += Transitioned of shown

(** A set of nodes, compared physically. *)
type nodes

external nodes : unit -> nodes = "Set" [@@mel.new]
external add : nodes -> Dom.node -> unit = "add" [@@mel.send]
external has : nodes -> Dom.node -> bool = "has" [@@mel.send]
external class_name : Dom.element -> string = "className" [@@mel.get]
external css_text : Dom.cssStyleDeclaration -> string = "cssText" [@@mel.get]
external style_value : Dom.cssStyleDeclaration -> string -> string = "" [@@mel.get_index]
external attribute : Dom.element -> string -> string Js.null = "getAttribute" [@@mel.send]

(** A running CSS transition or animation, as the Web Animations API gives it. *)
type animation

external animating : Dom.element -> (unit -> unit) Js.undefined = "getAnimations" [@@mel.get]
external animations : Dom.element -> animation array = "getAnimations" [@@mel.send]
external finished : animation -> unit Js.Promise.t = "finished" [@@mel.get]
external all : unit Js.Promise.t array -> unit Js.Promise.t = "all" [@@mel.scope "Promise"]

let running = "$$melhole:phase"

let stop element =
  Option.iter (fun stop -> stop ()) (Js.Undefined.toOption (Dom_api.property element running : (unit -> unit) Js.undefined))

let run ~name ~duration phase element ~finish =
  stop element;
  let classes = Dom_api.class_list element in
  let style = Dom_api.style element in
  let label part = name ^ "-" ^ part in
  let first, settled =
    match phase with
    | Entering -> ([ label "enter-from"; label "enter-active" ], [ label "enter-active"; label "enter-to" ])
    | Leaving -> ([ label "leave-from"; label "leave-active" ], [ label "leave-active"; label "leave-to" ])
    | Moving_by _ -> ([], [ label "move" ])
  in
  let own_transform = style_value style "transform" and own_duration = style_value style "transitionDuration" in
  let own_class = Js.Null.toOption (attribute element "class") and own_style = Js.Null.toOption (attribute element "style") in
  let unslide () =
    match phase with
    | Moving_by _ ->
        Dom_api.set_style style "transform" own_transform;
        Dom_api.set_style style "transitionDuration" own_duration
    | Entering | Leaving -> ()
  in
  let measured () =
    let computed = Dom_api.computed_style element in
    let times text =
      List.map
        (fun part ->
          let part = String.trim part in
          let time =
            match Js.String.endsWith ~suffix:"ms" part with
            | true -> Js.Float.fromString (Js.String.slice ~start:0 ~end_:(-2) part)
            | false -> 1000. *. Js.Float.fromString (Js.String.slice ~start:0 ~end_:(-1) part)
          in
          match Js.Float.isNaN time with true -> 0. | false -> time)
        (String.split_on_char ',' text)
    in
    let longest delays durations =
      let delays = Array.of_list (times delays) in
      List.fold_left Js.Math.max_float 0.
        (List.mapi
           (fun index duration -> duration +. Array.unsafe_get delays (index mod Array.length delays))
           (times durations))
    in
    int_of_float
      (Js.Math.max_float
         (longest computed##transitionDelay computed##transitionDuration)
         (longest computed##animationDelay computed##animationDuration))
  in
  let turn = ref Current in
  let cancel () =
    turn := Stale;
    List.iter (Dom_api.remove_class classes) first;
    List.iter (Dom_api.remove_class classes) settled;
    unslide ();
    (match (class_name element, own_class) with "", None -> Dom_api.remove_attribute element "class" | _, _ -> ());
    match (phase, css_text style, own_style) with
    | Moving_by _, "", None -> Dom_api.remove_attribute element "style"
    | Moving_by _, _, _ | (Entering | Leaving), _, _ -> ()
  in
  Dom_api.set_property element running (Js.Undefined.return cancel);
  List.iter (Dom_api.add_class classes) first;
  (match phase with
  | Moving_by (x, y) ->
      Dom_api.set_style style "transitionDuration" "0s";
      Dom_api.set_style style "transform" ("translate(" ^ Js.Float.toString x ^ "px, " ^ Js.Float.toString y ^ "px)")
  | Entering | Leaving -> ());
  let later step = Dom_api.request_animation_frame (fun _ -> match !turn with Current -> step () | Stale -> ()) in
  later (fun () ->
      later (fun () ->
          List.iter (Dom_api.remove_class classes) first;
          List.iter (Dom_api.add_class classes) settled;
          unslide ();
          let ended () =
            match !turn with
            | Current ->
                cancel ();
                Dom_api.set_property element running Js.undefined;
                finish ()
            | Stale -> ()
          in
          let timed wait = match wait with 0 -> ended () | wait -> Dom_api.set_timeout ended wait in
          match (duration, Js.Undefined.toOption (animating element)) with
          | Some wait, _ -> timed wait
          | None, None -> timed (measured ())
          | None, Some _ -> (
              match animations element with
              | [||] -> ended ()
              | running ->
                  let settle _ =
                    ended ();
                    Js.Promise.resolve ()
                  in
                  ignore (Js.Promise.catch settle (Js.Promise.then_ settle (all (Array.map finished running)))))))

let transition motion ~name ?duration child =
  let element node = match Dom_api.node_type node with 1 -> Some (Dom_api.unsafe_element node) | _ -> None in
  let top live =
    let found = ref [] in
    live.View.each (fun [@u] node -> found := node :: !found) [@u];
    Array.of_list (List.rev !found)
  in
  let wrap shown =
    View.live (Transitioned shown)
      ~each:(fun [@u] visit -> shown.inner.each visit [@u])
      ~front:(fun [@u] after -> shown.inner.front after [@u])
      ~release:(fun [@u] () ->
        List.iter
          (fun leaving ->
            stop leaving;
            Option.iter (fun parent -> Dom_api.remove_child parent leaving) (Dom_api.parent_node leaving))
          shown.leaving;
        shown.leaving <- [];
        shown.inner.release () [@u])
  in
  View.host
    ~build:(fun [@u] position -> wrap { name; motion; inner = View.build child position; leaving = [] })
    ~patch:(fun [@u] (position : Template.position) live ->
      match View.hosted live with
      | Some (Transitioned shown) when String.equal shown.name name && shown.motion == motion ->
          let placed node =
            match Js.Null.toOption node with
            | None -> Some node
            | Some node -> (
                match Dom_api.parent_node node with
                | Some parent when Dom_api.same parent position.parent -> Some (Js.Null.return node)
                | Some _ | None -> None)
          in
          let before = top shown.inner in
          let following = Array.map Dom_api.following_sibling before in
          let shown_before = nodes () in
          Array.iter (add shown_before) before;
          let places =
            match motion with
            | Still -> [||]
            | Sliding -> Array.map (fun node -> Option.map Dom_api.bounding_rect (element node)) before
          in
          shown.inner <- View.update position shown.inner child;
          let after = top shown.inner in
          let shown_after = nodes () in
          Array.iter (add shown_after) after;
          let place = ref position.before in
          for index = Array.length before - 1 downto 0 do
            let node = Array.unsafe_get before index in
            let here = Option.value (placed (Array.unsafe_get following index)) ~default:!place in
            (match (placed (Js.Null.return node), element node) with
            | None, Some leaving ->
                Dom_api.insert_before position.parent leaving here;
                Dom_api.set_attribute leaving "inert" "";
                shown.leaving <- leaving :: shown.leaving;
                run ~name ~duration Leaving leaving ~finish:(fun () ->
                    shown.leaving <- List.filter (fun other -> not (Dom_api.same other leaving)) shown.leaving;
                    Option.iter (fun parent -> Dom_api.remove_child parent leaving) (Dom_api.parent_node leaving))
            | Some _, _ | None, None -> ());
            place := here
          done;
          Array.iter
            (fun node ->
              match (has shown_before node, element node) with
              | false, Some entering -> run ~name ~duration Entering entering ~finish:ignore
              | true, _ | false, None -> ())
            after;
          Array.iteri
            (fun index rect ->
              let node = Array.unsafe_get before index in
              match (rect, has shown_after node) with
              | Some old, true -> (
                  let moved = Dom_api.unsafe_element node in
                  let fresh = Dom_api.bounding_rect moved in
                  match (old##left -. fresh##left, old##top -. fresh##top) with
                  | 0., 0. -> ()
                  | x, y -> run ~name ~duration (Moving_by (x, y)) moved ~finish:ignore)
              | Some _, false | None, _ -> ())
            places;
          Some live
      | Some _ | None -> None)

let make ~name ?duration child = transition Still ~name ?duration child
let group ~name ?duration child = transition Sliding ~name ?duration child
