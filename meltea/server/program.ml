type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread
type shape = Shape of string | Unshaped

let mutable_read = ()
let render ~view model = Melhole_server.render (Html.render (view model) ignore)
