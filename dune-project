(lang dune 3.6)

(name rvg)

(generate_opam_files true)

(source
 (github petervdonovan/rvg))

(authors "Peter Donovan")

; (maintainers "Maintainer Name")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name rvg)
 (synopsis "A little DSL for RISC-V assembly macros")
; (description "A longer description")
 (depends ocaml dune))
;  (tags
;   (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
