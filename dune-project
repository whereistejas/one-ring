(lang dune 3.23)

(name one-ring)

(generate_opam_files true)

(authors "The one-ring authors")

(maintainers "The one-ring maintainers")

(license LicenseRef-UNLICENSED)

(package
 (name one-ring)
 (synopsis "An AI workflow engine for composing and orchestrating agents")
 (description
  "one-ring is an AI workflow engine for defining, composing, and orchestrating agent-driven workflows.")
 (depends
  (ocaml (>= 5.1))
  yaml)
 (tags
  (ai agents workflows orchestration)))
