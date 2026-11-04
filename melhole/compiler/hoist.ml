open Ppxlib
open Ast_builder.Default

let counter = ref 0
let lifted = ref []

let hoist ~loc prefix expression =
  incr counter;
  let name = Printf.sprintf "__melhole_%s_%d" prefix !counter in
  lifted := [%stri let [%p pvar ~loc name] = [%e expression]] :: !lifted;
  evar ~loc name

let structure map items =
  List.concat_map
    (fun item ->
      let saved = !lifted in
      lifted := [];
      let item = map item in
      let before = List.rev !lifted in
      lifted := saved;
      before @ [ item ])
    items
