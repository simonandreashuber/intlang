(lang dune 3.21)

(name intlang)

(using menhir 3.0)



(generate_opam_files true)

(source
 (github username/reponame))

(authors "Simon Huber <hubersim@eth.ch>")

(maintainers "Simon Huber <hubersim@eth.ch>")

(license LICENSE)

(documentation https://github.com/simonandreashuber/intlang)

(package
 (name intlang)
 (synopsis "intlang is a small functional language")
 (description "intlang is a small functional language")
 (depends ocaml)
 (tags
  ("intlang")))

