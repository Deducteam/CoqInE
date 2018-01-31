CoqInE
======

### What is CoqInE?

CoqInE is a plugin for the [Coq](http://coq.inria.fr/) software translating Coq proofs into [Dedukti](https://www.rocq.inria.fr/deducteam/Dedukti/index.html) type-checkable terms.
It provides a signature file faithfully encoding the underlying theory of Coq (or a sufficiently large subset of it).
The generated ouput is meant to be type-checkable using the latest version of Dedukti.

It is developed by Gaspard Férey and François Thiré.
Current development is mostly focused on implementing support for Coq universe polymorphism.

Previous versions of the project can be found on:
* [InriaForge](https://gforge.inria.fr/projects/coqine/) in the `develop` branch.
* [Github](https://github.com/gburel/coqine)

Also check out [Krajono](https://gforge.inria.fr/projects/krajono/) project, a translation of [Matita](http://matita.cs.unibo.it/) proofs to Dedukti.

### Get CoqInE

Make sure the following dependencies are installed
* [OCaml](https://ocaml.org/docs/install.html)
* [Coq](https://github.com/coq/coq/wiki/Installation-of-Coq-on-Linux)
* [Dedukti](https://github.com/Deducteam/Dedukti/blob/master/README.md)

Warning: the version of OCaml used to compile CoqInE must be the same as the one used to compile the Coq program that generates the `.vo` files you want to translate.

1. Fork the project at `https://github.com/Deducteam/CoqInE/tree`
2. `coq_makefile -f Make -o CoqMakefile`
3. `make -f CoqMakefile`

### Using CoqInE

TODO


#### Documentation

The translation itself is explained in the following paper:

Mathieu Boespflug and Guillaume Burel.
*CoqInE: Translating the Calculus of Inductive Constructions into the lambda Pi-calculus Modulo*
, presented at the [PxTP'12](http://pxtp2012.inria.fr/) workshop.  [.pdf](http://www.ensiie.fr/~guillaume.burel/download/boespflug12coqine.pdf)
