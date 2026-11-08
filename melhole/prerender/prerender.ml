let () =
  let page early root path =
    Result.map
      (fun shell -> Shell.fill shell (In_channel.input_all stdin))
      (Shell.parse ~root early (In_channel.with_open_bin path In_channel.input_all))
  in
  match
    match Sys.argv with
    | [| _; "--capture"; root; path |] -> page Shell.Kept root path
    | [| _; root; path |] -> page Shell.Lost root path
    | _ -> Error `Usage
  with
  | Ok written -> print_string written
  | Error failure ->
      prerr_endline
        (match failure with
        | `Usage -> "usage: melhole-prerender [--capture] ROOT_ID SHELL.html < app.html > index.html"
        | `Shell (Shell.No_root root) -> "melhole-prerender: no element has the id " ^ root
        | `Shell (Shell.Filled_root root) -> "melhole-prerender: the element " ^ root ^ " is not empty"
        | `Shell Shell.No_head -> "melhole-prerender: no </head> before the root, for the capture script");
      exit 2
