(lang dune 3.3)

(name aoc2022)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Arvid Jakobsson")

(maintainers "Arvid Jakobsson <arvid.jakobsson@gmail.com>")

(license MIT)

(documentation https://github.com/arvidj/aoc2022)

(package
 (name aoc2022)
 (synopsis "Entry in Advent of code 2022")
 (description "Entry in Advent of code 2022")
 (depends ocaml dune ppx_expect)
 (tags
  (topics "aoc2022")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
