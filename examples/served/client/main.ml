external document : Dom.document = "document"

external element_by_id : Dom.document -> string -> Dom.element option = "getElementById"
[@@mel.send] [@@mel.return nullable]

external data_count : Dom.element -> string = "count" [@@mel.get] [@@mel.scope "dataset"]

let () =
  Option.iter
    (fun root ->
      ignore
        (Program.sandbox ~start:Program.hydrate ~init:(int_of_string (data_count root)) ~update:Page.update
           ~view:Page.view root))
    (element_by_id document "root")
