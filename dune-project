(lang dune 1.10)
(using coq 0.1)
(name coqine)
(version dev)

(generate_opam_files true)

(maintainers   "dedukti-dev@inria.fr")
(authors       "Ali Assaf, Mathieu Boespflug, Guillaume Burel, Quentin Carbonneaux, Gaspard Férey and François Thiré")
(homepage      "https://github.com/Deducteam/CoqInE/")
(source (github Deducteam/CoqInE))
(license       "GPL 3")


(package
  (name coqine)
  (depends
    (ocaml (<= 4.14.0))
    (rocq-prover (= 9.0.0))
    (dune (>= 1.9.0)))
  (synopsis "The Coqine Plugin")
  (description "A translator from the Rocq Prover to Dedukti"))
