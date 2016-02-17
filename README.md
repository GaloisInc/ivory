[![Build Status](https://travis-ci.org/GaloisInc/ivory.svg?branch=master)](https://travis-ci.org/GaloisInc/ivory)

# [Ivory][ivory]

Ivory is an embedded domain specific language (EDSL) which aims to provide
a systems-level programming language that removes some common pitfalls of
programming in C, without sacrificing expressivity.

This repository includes a [user guide][userguide] and some
[examples][examples] Ivory programs. More information and tutorials are
available on [ivorylang.org](http://ivorylang.org).


## Contents

* `ivory`: the Ivory language implementation and interpreter
* `ivory-backend-c`: a backend for compiling Ivory programs to C
* `ivory-examples`: sample Ivory programs
* `ivory-opts`: an optimization framework and some optimization
  implementations, for the Ivory AST.
* `ivory-bitdata`: a macro language library for specifying bit-precise
  Ivory operations.
* `ivory-hw`: a macro language library for writing hardware drivers
  in Ivory.
* `ivory-model-check`: a backend for verifying Ivory programs with CVC4

## Installing

Ivory is written in Haskell and uses several recent GHC extensions.  It is known
to work with with the GHC 7.8.x series.

Ivory is not yet compatible with GHC 7.10. If this prevents you from making use
of Ivory, please file a bug on github telling us so.

We recommend using the [Stack][stack] build tool for Ivory language
packages and any programs which use them.

## Copyright and license
Copyright 2013-2015 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

[ivory]: http://github.com/GaloisInc/ivory
[userguide]: http://github.com/GaloisInc/ivory/blob/master/ivory/user-guide.md
[examples]: http://github.com/GaloisInc/ivory/tree/master/ivory-examples/examples
[stack]: http://www.haskellstack.org/
[galois]: http://galois.com

## Contributing

This project adheres to the
[Contributor Covenant code of conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unaccpetable
behavior to [smaccm@galois.com](mailto:smaccm@galois.com).
