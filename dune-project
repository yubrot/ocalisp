(lang dune 3.15)

(name ocalisp)

(generate_opam_files true)

(authors "yubrot")

(maintainers "yubrot")

(license BSD-3-Clause)

(source
 (github yubrot/ocalisp))

(package
 (name ocalisp)
 (synopsis "A tiny Lisp-1 implementation")
 (description "A tiny Lisp-1 implementation")
 (depends
  (ocaml
   (>= 4.06.0))
  dune
  menhir))

(using menhir 2.0)
