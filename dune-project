(lang dune 3.19)
(using dune_site 0.1)

(name forester_ocaml_plugin)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ilias Garnier <igarnier@protonmail.com>")

(maintainers "Ilias Garnier <igarnier@protonmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name forester_ocaml_plugin)
 (synopsis "Run OCaml code in Forester")
 (description "A plugin to execute and render snippets of OCaml in Forester")
 (depends
   ocaml
   (forester (= "5.0")))
 (tags
  ("forester" "plugin")))