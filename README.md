CoqInE
======

### What is CoqInE?

CoqInE is a plugin for the [Coq](http://coq.inria.fr/) software translating Coq proofs into [Dedukti](https://www.rocq.inria.fr/deducteam/Dedukti/index.html) type-checkable terms.
It provides a signature file faithfully encoding the underlying theory of Coq (or a sufficiently large subset of it).
The generated ouput is meant to be type-checkable using the latest version of Dedukti.

It is developed by Mathieu Boespflug, Guillaume Burel, Quentin Carbonneaux, Ali Assaf and Gaspard Férey.
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

Download [this file](http://www.ensiie.fr/~guillaume.burel/download/coqine.tar.gz). Unzip and extract it 

    tar xvzf coqine.tar.gz

Change directory

    cd coqine

Then configure and compile CoqInE:

    ./configure
    make

(Use `./configure -help` for available configuration options.)

Start enjoying

    cd t
    coqc test.v
    ../bin/coqine -h test.vo
    dedukti Coq1univ.dk test.dk | lua -l dedukti -

#### Documentation

A succinct (outdated) manual is provided with the sources (`doc/coqine.1.gz`).
The translation itself is explained in the following paper:

Mathieu Boespflug and Guillaume Burel.
*CoqInE: Translating the Calculus of Inductive Constructions into the lambda Pi-calculus Modulo*
, presented at the [PxTP'12](http://pxtp2012.inria.fr/) workshop.  [.pdf](http://www.ensiie.fr/~guillaume.burel/download/boespflug12coqine.pdf)
