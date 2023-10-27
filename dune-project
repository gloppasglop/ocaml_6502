(lang dune 3.10)

(name ocaml_6502)

(generate_opam_files true)

(source
 (github gloppasglop/ocaml_6502))

(authors "Christophe Roux")

(maintainers "Christophe Roux")

(license MIT)

(documentation https://github.com/gloppasglop/ocaml_6502)

(package
 (name ocaml_6502)
 (synopsis "A 6502 emulator in ocaml")
 (description "A 6502 emulator in ocaml")
 (depends ocaml dune)
 (tags
  (emulator 6502 c64)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
