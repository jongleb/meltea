(** How the handler property of an element is reached by the browser. *)
type registration =
  | Delegated  (** One [document] listener walks up from the target. *)
  | Direct of (Dom.event -> unit)  (** A listener on the element itself. *)

type t = {
  element : Dom.element;
  event : string;
  key : string;
  registration : registration;
}

let delegated_types = Hashtbl.create 32

let bind element event handler =
  let key = "$$melhole:" ^ event in
  Dom_api.set_handler element key handler;
  let registration =
    match event with
    | "beforeinput" | "click" | "dblclick" | "contextmenu" | "focusin"
    | "focusout" | "input" | "keydown" | "keyup" | "mousedown" | "mousemove"
    | "mouseout" | "mouseover" | "mouseup" | "pointerdown" | "pointermove"
    | "pointerout" | "pointerover" | "pointerup" | "touchend" | "touchmove"
    | "touchstart" ->
        (match Hashtbl.find_opt delegated_types event with
        | Some () -> ()
        | None ->
            Hashtbl.add delegated_types event ();
            Dom_api.add_document_listener Dom_api.document event (fun native ->
                let rec bubble node =
                  Option.iter (fun handle -> handle native)
                    (Dom_api.handler node key);
                  match Dom_api.cancel_bubble native with
                  | true -> ()
                  | false -> Option.iter bubble (Dom_api.parent_node node)
                in
                Option.iter bubble (Dom_api.target native)));
        Delegated
    | _ ->
        let listener native =
          Option.iter (fun handle -> handle native)
            (Dom_api.handler (Dom_api.to_node element) key)
        in
        Dom_api.add_listener element event listener;
        Direct listener
  in
  { element; event; key; registration }

let set t handler = Dom_api.set_handler t.element t.key handler

let release t =
  match t.registration with
  | Delegated -> ()
  | Direct listener -> Dom_api.remove_listener t.element t.event listener
