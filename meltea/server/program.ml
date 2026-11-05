type 'msg changes =
  | Changes : { analysed : 'update; fields : 'msg -> int } -> 'msg changes
  | Unanalysed

type reads = Reads : { analysed : 'view; fields : int } -> reads | Unread

let mutable_read = ()
let render ~view model = Melhole_server.render (Html.render (view model) ignore)
