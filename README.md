functionalKleene
================

A library for computing Kleene closures of matrices.

Installation with Cabal
-----------------------

The simplest way is using Cabal which is part of the
[Haskell Platform](http://www.haskell.org/platform/).

If you are using git, then simply clone the project into a folder of your choice.
Otherwise download the zipped version and unpack it into a folder of your choice.

Assuming, all project files are located in `~/functionalKleene` you can proceed as follows.

~~~{.sh}
bash> cd ~/functionalKleene
bash> cabal update
bash> cabal install
~~~

The project is now installed and can be either used directly, e.g.:

~~~{.sh}
bash> ghci
ghci> :m FunctionalKleene
FunctionalKleene>
~~~

or imported into an own Haskell module via `import FunctionalKleene`.

Documentation
-------------

You can browse the documentation online:

* [FunctionalKleene](./src/FunctionalKleene.lhs)
  is the main file. It contains the algorithms.
* [RandomMatrix](./src/RandomMatrix.lhs)
  is an auxiliary file that takes care of the creation of random matrices.
* [KleeneAlgebra](./src/KleeneAlgebra.lhs)
  is another auxiliary file that contains the Kleene algebra type class and some common instances.

Alternatively, you can view these files locally as `.lhs` files directly or as `.md` files

* [FunctionalKleene](./src/FunctionalKleene.md)
* [RandomMatrix](./src/RandomMatrix.md)
* [KleeneAlgebra](./src/KleeneAlgebra.md)

All files are located in the `/src` folder.