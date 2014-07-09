# A formal semantics for Ivory 

This directory contains the Isabelle/HOL theory files for the
formal model of Ivory, along with proofs of type safety.

## Contents

* `Lib.thy`:  Lemmas which are not Ivory specific, and could/should be
  in the standard library
* `Heaps.thy`:  Definitions and lemmas for manipulating Ivory heaps and heap types 
* `Syntax.thy`:  The syntax of Ivory terms and types
* `Semantics.thy`:  An abstract machine for the semantics of Ivory
* `TypeSystem.thy`:  The model of the type system 
* `TypeSystemProps.thy`:  Properties of type system judgements 
* `EvalSafe.thy`:  Proofs about the safety of expression evaluation 
* `Progress.thy`:  The proof of the progress property 
* `Preservation.thy`:  The proof of the preservation property 
* `Soundness.thy`:  The top-level proofs of soundness 

## Installing

This proof was developed with Isabelle version 2013-2.  The theory
file `Soundness.thy` contains the toplevel theorems.

## Copyright and license
Copyright 2013-2014 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

[ivory]: http://github.com/GaloisInc/ivory
[galois]: http://galois.com
