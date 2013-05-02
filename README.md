# [Ivory][ivory]

Ivory is an embedded domain specific language (EDSL) which aims to provide
a systems-level programming language that removes some common pitfalls of
programming in C, without sacrificing expressivity.

This repository includes a [user guide][userguide] and some [examples][examples] of simple
Ivory programs.

## Installing

Ivory is written in Haskell and uses several recent GHC extensions.
It is known to work with with GHC 7.6.2.

We recommend using [cabal-dev][cabaldev] to manage a sandbox containing these
Ivory language packages and any programs which use them.

## Copyright and license
Copyright 2013 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

[ivory]: http://github.com/GaloisInc/ivory
[userguide]: http://github.com/GaloisInc/ivory/blob/master/ivory/user-guide.md
[examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples/examples
[cabaldev]: http://hackage.haskell.org/package/cabal-dev
[galois]: http://galois.com
