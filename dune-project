(lang dune 3.15)

(name simple_llvm)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github glyh/simple_llvm))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/simple_llvm)

(package
 (name simple_llvm)
 (synopsis "A simple language implemented with OCaml and LLVM")
 (description "A simple language implemented with OCaml and LLVM")
 (depends
   ocaml
   dune
   menhir
   (ppx_jane (>= v0.17.0))
   (core (>= v0.17.1))
   (alcotest :with-test)
   (ppx_deriving_protobuf (>= 3.0.0)))
   
 (tags
  (topics compiler LLVM)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
