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
let deliveries = ref []

let key_of event =
  match List.find_map (fun (known, key) -> if String.equal known event then Some key else None) !keys with
  | Some key -> key
  | None ->
      let key = "$$melhole:" ^ event in
      keys := (event, key) :: !keys;
      key

let controlled = "$$melhole:controlled"

let register element event =
  let key = key_of event in
  match event with
    | "beforeinput" | "change" | "click" | "dblclick" | "contextmenu" | "focusin"
    | "focusout" | "input" | "keydown" | "keyup" | "mousedown" | "mousemove"
    | "mouseout" | "mouseover" | "mouseup" | "pointerdown" | "pointermove"
    | "pointerout" | "pointerover" | "pointerup" | "touchend" | "touchmove"
    | "touchstart" ->
        (match List.assoc_opt event !deliveries with
        | Some _ -> ()
        | None ->
            let pointing =
              match event with
              | "click" | "dblclick" | "mousedown" | "mousemove" | "mouseup" -> `Mouse
              | _ -> `Other
            in
            let deliver native =
              let rec bubble node =
                Option.iter
                  (fun handle ->
                    match (pointing, Dom_api.node_name node) with
                    | `Mouse, ("BUTTON" | "INPUT" | "SELECT" | "TEXTAREA")
                      when (Dom_api.property (Dom_api.unsafe_element node) "disabled" : bool) ->
                        ()
                    | (`Mouse | `Other), _ ->
                        Dom_api.point_current_target native [%mel.obj { configurable = true; value = node }];
                        paced event handle native)
                  (Dom_api.handler node key);
                match Dom_api.cancel_bubble native with
                | true -> ()
                | false -> Option.iter bubble (Dom_api.parent_node node)
              in
              Option.iter bubble (Dom_api.target native);
              Dom_api.release_current_target native;
              match event with
              | "input" | "change" ->
                  Option.iter
                    (fun target -> Option.iter (fun restore -> restore native) (Dom_api.handler target controlled))
                    (Dom_api.target native)
              | _ -> ()
            in
            deliveries := (event, deliver) :: !deliveries;
            Dom_api.add_document_listener Dom_api.document event deliver);
        Delegated
    | _ ->
        let listener native =
          Option.iter (fun handle -> paced event handle native) (Dom_api.handler (Dom_api.to_node element) key)
        in
        Dom_api.add_listener element event listener;
        Direct (fun () -> Dom_api.remove_listener element event listener)

let set element key handler = Dom_api.set_handler element key handler

external early : (unit -> (Dom.event * string Js.undefined * bool Js.undefined) array) Js.undefined
  = "__melhole_early"
[@@mel.scope "globalThis"]

external event_type : Dom.event -> string = "type" [@@mel.get]
external contains : Dom.node -> Dom.node -> bool = "contains" [@@mel.send]
external value : Dom.node -> string Js.undefined = "value" [@@mel.get]
external checked : Dom.node -> bool Js.undefined = "checked" [@@mel.get]
external set_value : Dom.node -> string -> unit = "value" [@@mel.set]
external set_checked : Dom.node -> bool -> unit = "checked" [@@mel.set]

let replay root =
  let restore target value checked =
    Option.iter (set_value target) (Js.Undefined.toOption value);
    Option.iter (set_checked target) (Js.Undefined.toOption checked)
  in
  Option.iter
    (fun take ->
      let recorded = take () in
      let finals =
        Array.map
          (fun (native, _, _) ->
            Option.map (fun target -> (target, value target, checked target)) (Dom_api.target native))
          recorded
      in
      Array.iter
        (fun (native, early_value, early_checked) ->
          Option.iter
            (fun target ->
              match (contains root target, List.assoc_opt (event_type native) !deliveries) with
              | false, _ -> ()
              | true, found -> (
                  restore target early_value early_checked;
                  match found with
                  | Some deliver -> deliver native
                  | None ->
                      let key = key_of (event_type native) in
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
                  match event_type native with
                  | "input" | "change" -> Option.iter (fun restore -> restore native) (Dom_api.handler target controlled)
                  | _ -> ())
            (Dom_api.target native))
        recorded;
      Array.iter (Option.iter (fun (target, value, checked) -> restore target value checked)) finals)
    (Js.Undefined.toOption early)
