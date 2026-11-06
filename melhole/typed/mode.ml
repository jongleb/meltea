type t = Untyped | Typed

let chosen = ref Untyped
let current () = !chosen

let () =
  Ppxlib.Driver.add_arg "-typed"
    (Arg.Unit (fun () -> chosen := Typed))
    ~doc:" Type the file after rewriting it, for code and checks that need types (a staged rewriter only)"
