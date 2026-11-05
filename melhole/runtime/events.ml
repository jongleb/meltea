type registration = Delegated | Direct of (unit -> unit)

let keys = ref []
let delegated_types = ref []

let bind element event =
  let key =
    match List.find_map (fun (known, key) -> if String.equal known event then Some key else None) !keys with
    | Some key -> key
    | None ->
        let key = "$$melhole:" ^ event in
        keys := (event, key) :: !keys;
        key
  in
  let registration =
    match event with
    | "beforeinput" | "click" | "dblclick" | "contextmenu" | "focusin"
    | "focusout" | "input" | "keydown" | "keyup" | "mousedown" | "mousemove"
    | "mouseout" | "mouseover" | "mouseup" | "pointerdown" | "pointermove"
    | "pointerout" | "pointerover" | "pointerup" | "touchend" | "touchmove"
    | "touchstart" ->
        (match List.find_opt (String.equal event) !delegated_types with
        | Some _ -> ()
        | None ->
            delegated_types := event :: !delegated_types;
            Dom_api.add_document_listener Dom_api.document event (fun native ->
                let rec bubble node =
                  Option.iter
                    (fun handle ->
                      Dom_api.point_current_target native [%mel.obj { configurable = true; value = node }];
                      handle native)
                    (Dom_api.handler node key);
                  match Dom_api.cancel_bubble native with
                  | true -> ()
                  | false -> Option.iter bubble (Dom_api.parent_node node)
                in
                Option.iter bubble (Dom_api.target native);
                Dom_api.release_current_target native));
        Delegated
    | _ ->
        let listener native =
          Option.iter (fun handle -> handle native) (Dom_api.handler (Dom_api.to_node element) key)
        in
        Dom_api.add_listener element event listener;
        Direct (fun () -> Dom_api.remove_listener element event listener)
  in
  (key, registration)

let set element key handler = Dom_api.set_handler element key handler
