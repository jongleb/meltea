type t = Delegated | Direct

let of_event event =
  match event with
  | "beforeinput" | "change" | "click" | "dblclick" | "contextmenu" | "focusin" | "focusout" | "input"
  | "keydown" | "keyup" | "mousedown" | "mousemove" | "mouseout" | "mouseover" | "mouseup" | "pointerdown"
  | "pointermove" | "pointerout" | "pointerover" | "pointerup" | "touchend" | "touchmove" | "touchstart"
  | "onchange" ->
      Delegated
  | _ -> Direct
