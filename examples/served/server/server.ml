let () =
  let count = match Sys.argv with [| _; count |] -> int_of_string count | _ -> 41 in
  print_string
    ("<!doctype html><html><head><meta charset=\"utf-8\"><script>" ^ Melhole_server.capture
   ^ "</script></head><body><div id=\"root\" data-count=\"" ^ string_of_int count ^ "\">"
   ^ Program.render ~view:Page.view count
   ^ "</div><script type=\"module\" src=\"../client/output/examples/served/client/main.mjs\"></script></body></html>")
