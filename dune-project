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
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.16))
  (melange (>= 5.1))
  (ppxlib (>= 0.34))
  ppx_deriving_variant_string
  ppx_enumerate
  (qcheck-core :with-test)))

(package
 (name meltea)
 (synopsis "The Elm architecture for Melange: pure functions, mlx views, compiled templates")
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.16))
  (melange (>= 5.1))
  melhole
  (mlx :with-test)
  (jsonkit-melange :with-test)))

(package
 (name meltea-ppx)
 (synopsis "mlx views with messages, compiled to templates, for Meltea")
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.16))
  (ppxlib (>= 0.34))
  melhole))
