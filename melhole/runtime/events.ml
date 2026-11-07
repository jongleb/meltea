type registration = Delegated | Direct of (unit -> unit)
type pace = Discrete | Continuous

let pace = ref Discrete

let paced event handle native =
  match event with
  | "mousemove" | "pointermove" | "touchmove" | "scroll" | "wheel" | "drag" | "dragover" ->
      pace := Continuous;
      (match handle native with
      | () -> pace := Discrete
      | exception error ->
          pace := Discrete;
          raise error)
  | _ -> handle native

let keys = ref []

let key_of event =
  match List.find_map (fun (known, key) -> if String.equal known event then Some key else None) !keys with
  | Some key -> key
  | None ->
      let key = "$$melhole:" ^ event in
      keys := (event, key) :: !keys;
      key

let controlled = "$$melhole:controlled"

(** A JavaScript [Map] from a delegated event to the [document] listener that delivers it. *)
type deliveries

external deliveries_made : unit -> deliveries = "Map" [@@mel.new]
external delivery : deliveries -> string -> (Dom.event -> unit) Js.undefined = "get" [@@mel.send]
external deliver_with : deliveries -> string -> (Dom.event -> unit) -> unit = "set" [@@mel.send]

let deliveries = deliveries_made ()

let tracked = "$$melhole:tracked"

let shown element =
  match Js.Undefined.toOption (Dom_api.property element "type" : string Js.undefined) with
  | Some ("checkbox" | "radio") -> (match (Dom_api.property element "checked" : bool) with true -> "on" | false -> "off")
  | Some _ | None -> Option.value ~default:"" (Js.Undefined.toOption (Dom_api.property element "value" : string Js.undefined))

let track element = Dom_api.set_property element tracked (shown element)

let delegate event =
  match Js.Undefined.toOption (delivery deliveries event) with
  | Some _ -> ()
  | None -> (
      let key = key_of event in
      let pointing =
        match event with "click" | "dblclick" | "mousedown" | "mousemove" | "mouseup" -> `Mouse | _ -> `Other
      in
      let tracking = match event with "onchange" -> `Tracking | _ -> `Untracked in
      let deliver native =
        let rec bubble node =
          Option.iter
            (fun handle ->
              match (pointing, Dom_api.node_name node) with
              | `Mouse, ("BUTTON" | "INPUT" | "SELECT" | "TEXTAREA")
                when (Dom_api.property (Dom_api.unsafe_element node) "disabled" : bool) ->
                  ()
              | (`Mouse | `Other), _ -> (
                  let deliver () =
                    Dom_api.point_current_target native [%mel.obj { configurable = true; value = node }];
                    paced event handle native
                  in
                  match tracking with
                  | `Untracked -> deliver ()
                  | `Tracking -> (
                      let element = Dom_api.unsafe_element node in
                      let now = shown element in
                      match Js.Undefined.toOption (Dom_api.property element tracked : string Js.undefined) with
                      | Some before when String.equal before now -> ()
                      | Some _ | None ->
                          Dom_api.set_property element tracked now;
                          deliver ())))
            (Dom_api.handler node key);
          match Dom_api.cancel_bubble native with
          | true -> ()
          | false -> Option.iter bubble (Dom_api.parent_node node)
        in
        Option.iter bubble (Dom_api.target native);
        Dom_api.release_current_target native;
        match event with
        | "input" | "change" | "onchange" ->
            Option.iter
              (fun target -> Option.iter (fun restore -> restore native) (Dom_api.handler target controlled))
              (Dom_api.target native)
        | _ -> ()
      in
      deliver_with deliveries event deliver;
      match event with
      | "onchange" ->
          Dom_api.add_document_listener Dom_api.document "input" deliver;
          Dom_api.add_document_listener Dom_api.document "change" deliver
      | _ -> Dom_api.add_document_listener Dom_api.document event deliver)

let register element event =
  match Melhole_shared.Delegation.of_event event with
  | Delegated ->
      delegate event;
      Delegated
  | Direct ->
      let key = key_of event in
      let listener native =
        Option.iter (fun handle -> paced event handle native) (Dom_api.handler (Dom_api.to_node element) key)
      in
      Dom_api.add_listener element event listener;
      Direct (fun () -> Dom_api.remove_listener element event listener)

let set element key handler = Dom_api.set_handler element key handler

let removals = ref []

let removal_of event =
  match List.find_map (fun (known, key) -> if String.equal known event then Some key else None) !removals with
  | Some key -> key
  | None ->
      let key = "$$melhole:off:" ^ event in
      removals := (event, key) :: !removals;
      key

let listen element event =
  match register element event with
  | Delegated -> ()
  | Direct remove -> Dom_api.set_property element (removal_of event) remove

let unlisten element event =
  match Melhole_shared.Delegation.of_event event with
  | Delegated -> ()
  | Direct ->
      Option.iter
        (fun remove -> remove ())
        (Js.Undefined.toOption (Dom_api.property element (removal_of event) : (unit -> unit) Js.undefined))

external capture : (unit -> (Dom.event * string Js.undefined * bool Js.undefined) array) Js.undefined
  = "__melhole_early"
[@@mel.scope "globalThis"]

external contains : Dom.node -> Dom.node -> bool = "contains" [@@mel.send]
external value : Dom.node -> string Js.undefined = "value" [@@mel.get]
external checked : Dom.node -> bool Js.undefined = "checked" [@@mel.get]
external set_value : Dom.node -> string -> unit = "value" [@@mel.set]
external set_checked : Dom.node -> bool -> unit = "checked" [@@mel.set]

type early = { root : Dom.node; kept : (Dom.event * string Js.undefined * bool Js.undefined) array }

let early root =
  Option.bind (Js.Undefined.toOption capture) (fun take ->
      match
        Js.Array.filter
          ~f:(fun (native, _, _) -> Option.fold ~none:false ~some:(contains root) (Dom_api.target native))
          (take ())
      with
      | [||] -> None
      | kept -> Some { root; kept })

let restore target kept_value kept_checked =
  Option.iter
    (fun kept -> if not (Js.Undefined.toOption (value target) = Some kept) then set_value target kept)
    (Js.Undefined.toOption kept_value);
  Option.iter
    (fun kept -> if not (Js.Undefined.toOption (checked target) = Some kept) then set_checked target kept)
    (Js.Undefined.toOption kept_checked)

let preserving native run =
  Option.iter
    (fun target ->
      let value = value target and checked = checked target in
      run ();
      restore target value checked)
    (Dom_api.target native)

let replay { root; kept } =
  let finals =
    Array.map
      (fun (native, _, _) -> Option.map (fun target -> (target, value target, checked target)) (Dom_api.target native))
      kept
  in
  Array.iter
    (fun (native, early_value, early_checked) ->
      Option.iter
        (fun target ->
          match (contains root target, Js.Undefined.toOption (delivery deliveries (Dom_api.event_type native))) with
          | false, _ -> ()
          | true, found -> (
              restore target early_value early_checked;
              match found with
              | Some deliver -> deliver native
              | None ->
                  let key = key_of (Dom_api.event_type native) in
                  let rec bubble node =
                    Option.iter
                      (fun handle ->
                        Dom_api.point_current_target native [%mel.obj { configurable = true; value = node }];
                        handle native)
                      (Dom_api.handler node key);
                    match (Dom_api.cancel_bubble native, Dom_api.same node root) with
                    | true, _ | false, true -> ()
                    | false, false -> Option.iter bubble (Dom_api.parent_node node)
                  in
                  bubble target;
                  Dom_api.release_current_target native);
              match Dom_api.event_type native with
              | "input" | "change" ->
                  Option.iter (fun deliver -> deliver native) (Js.Undefined.toOption (delivery deliveries "onchange"));
                  Option.iter (fun restore -> restore native) (Dom_api.handler target controlled)
              | _ -> ())
        (Dom_api.target native))
    kept;
  Array.iter (Option.iter (fun (target, value, checked) -> restore target value checked)) finals
