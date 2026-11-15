(lang dune 3.16)

(using melange 0.1)

(name meltea)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(generate_opam_files true)

(license MIT)

(authors "Gleb")

(maintainers "Gleb")

(source
 (github jongleb/meltea))

(package
 (name melhole)
 (synopsis "Templates with holes for Melange: the runtime and the compiler shared by Meltea and Melact")
 (description
  "melhole turns a view known at build time into a DOM template with holes and per-template update code, guarded by === on the values each hole reads. It is the runtime and the compiler under Meltea and Melact, and has a server writer that renders the same views to HTML natively.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.16))
  (melange (>= 7.0.0))
  (ppxlib (>= 0.39.0))
  ppx_deriving_variant_string
  ppx_enumerate
  (qcheck-core :with-test)))

(package
 (name meltea)
 (synopsis "The Elm architecture for Melange: pure functions, mlx views, compiled templates")
 (description
  "Meltea is the Elm architecture in OCaml: a model, messages, a pure update and a pure view written in mlx, compiled by meltea-ppx to DOM templates that re-render only what a message changed.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.16))
  (melange (>= 7.0.0))
  (melhole (= :version))
  (meltea-ppx (and :with-test (= :version)))
  (mlx :with-test)
  (jsonkit-melange :with-test)))

(package
 (name meltea-ppx)
 (synopsis "Meltea's mlx views with messages, compiled to templates")
 (description
  "meltea-ppx compiles Meltea's mlx views to melhole templates, reads update and view for the model fields each message changes, and can write a view's HTML natively for server rendering.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.16))
  (ppxlib (>= 0.39.0))
  (melhole (= :version))
  (qcheck-core :with-test)))
