external document : Dom.document = "document"

external element_by_id : Dom.document -> string -> Dom.element option = "getElementById"
[@@mel.send] [@@mel.return nullable]

external parse : string -> int = "parse" [@@mel.scope "JSON"]

let () =
  Option.iter
    (fun root ->
      ignore
        (Program.sandbox ~start:Program.hydrate
           ~init:(Option.fold ~none:0 ~some:parse (Melhole.Served.data ~id:"model"))
           ~update:Page.update ~view:Page.view root))
    (element_by_id document "root")
