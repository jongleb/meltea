type t = Delegated | Crossing | Direct

let of_event event =
  match event with
  | "beforeinput" | "change" | "click" | "dblclick" | "contextmenu" | "focusin" | "focusout" | "input"
  | "keydown" | "keyup" | "mousedown" | "mousemove" | "mouseout" | "mouseover" | "mouseup" | "pointerdown"
  | "pointermove" | "pointerout" | "pointerover" | "pointerup" | "touchend" | "touchmove" | "touchstart"
  | "onchange" ->
      Delegated
  | "mouseenter" | "mouseleave" | "pointerenter" | "pointerleave" -> Crossing
  | _ -> Direct

let compiled event =
  match event with
  | "submit" | "reset" | "wheel" | "keypress" | "auxclick" | "copy" | "cut" | "paste" | "compositionstart"
  | "compositionupdate" | "compositionend" | "select" | "pointercancel" | "touchcancel" | "gotpointercapture"
  | "lostpointercapture" | "drag" | "dragstart" | "dragend" | "dragenter" | "dragleave" | "dragover" | "drop"
  | "animationstart" | "animationend" | "animationiteration" | "transitionstart" | "transitionrun"
  | "transitionend" | "transitioncancel" ->
      Delegated
  | _ -> of_event event
