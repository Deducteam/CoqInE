CoqInE
======

### What is CoqInE?

CoqInE is a plugin for the [Coq](http://coq.inria.fr/) software translating Coq proofs into [Dedukti](https://www.rocq.inria.fr/deducteam/Dedukti/index.html) type-checkable terms.
It provides a signature file faithfully encoding the underlying theory of Coq (or a sufficiently large subset of it).
The generated ouput is meant to be type-checkable using the latest version of Dedukti.

It was developed by Ali Assaf, Mathieu Boespflug, Guillaume Burel, Quentin Carbonneaux, Gaspard Férey and François Thiré.
It is currently developed by Gaspard Férey and François Thiré.
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

Installation steps:
```
git clone https://github.com/Deducteam/CoqInE.git
cd CoqInE
make
```

Adding the following line to your `~\.coqrc` file can help solve some issues (`/path/to/coqine` should be replaced with the actual path to your CoqInE repository folder).
```
Add ML Path "/path/to/coqine/master/src".
```


### Using CoqInE (TODO)

Examples of library translations are located in the `run` folder.
* To build debug files: `make debug`
* To build test files: `make test`
* To build part of the [GeoCoq](https://github.com/GeoCoq/GeoCoq) library: `make geocoq`

Generated `.dk` files are in the corresponding `out` folder.


#### Documentation

A succinct (outdated) manual is provided with the sources (`doc/coqine.1.gz`).
The translation itself is explained in the following paper:

Mathieu Boespflug and Guillaume Burel.
*CoqInE: Translating the Calculus of Inductive Constructions into the lambda Pi-calculus Modulo*
, presented at the [PxTP'12](http://pxtp2012.inria.fr/) workshop.  [.pdf](http://www.ensiie.fr/~guillaume.burel/download/boespflug12coqine.pdf)

### Using CoqInE with Nix

CoqInE can be tested using [nix](https://nixos.org/). Run `nix build` to build
CoqInE, `nix develop ".#extraction"` to get a shell with CoqInE installed, and
`nix flake check` to run the tests.
