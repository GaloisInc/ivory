# Ivory Overview

## Expressions

### Integer Types

The integer types available to the Ivory programmer are, by design, the same
integers made available by `stdint.h` in C. Integers are available in 8, 16, 32,
and 64 bit widths, in both signed and unsigned variants - an unsigned 8 bit
integer has the Haskell type `Uint8`, a signed 16 bit integer has type `Sint16`,
and so on. Mathematical operations are available to the user through the Haskell
`Num` typeclass. Additionally, Ivory provides operators for bitwise logic and
shift operations with the semantics C programmers expect.

The major difference between integer expressions in C and integer
expressions in Ivory is that, like all conversions between Haskell values of
different types, conversions are explicit. For conversions where no loss of
information is possible, such as from a `Uint8` to a `Sint32`, the `safeCast`
primitive is available. For conversions where information may be lost, such as
from a `Uint32` to `Uint16`, the user can choose from a number of casts - 

For conversions between signed and unsigned types of the same width, the
`twosComplementCast` primitive is available, which will convert `Uint32` to
`Sint32` bitwise, and `signCast`, which will convert between two numerical
values, with a default of the result type's low or high bound when the result
type cannot represent a given numerical value.


### Floating Point Types

The C types `float` and `double` are available to Ivory programmers via
the Haskell types `IFloat` and `IDouble`. These are also instances of the `Num`
typeclass and 

### Boolean Types

Ivory provides a boolean type called `IBool`, and two values `true` and `false`
in of that type. In order to implement Ivory correctly, it cannot reuse
Haskell's existing `Bool` type.

As a consequence, Ivory must use a different name for any Haskell operations
over Ivory types that give a boolean result - for example, Ivory cannot
implement the `Eq` class for Sint8 (`(==) :: Sint8 -> Sint8 -> Bool`), but
instead an `IvoryEq` class with an operator `(==?) :: Sint8 -> Sint8 -> IBool`.
Other comparisons, e.g. those in `IvoryOrd`, are also given their ordinary
haskell names postfixed with a `?`.

### Array types

TK TK they can have any stored thign inside them, and length is always specified
using a type nat in the type.

Array offsets are specified by the type `Ix n`. In general, the `Ix n` type may
be used for any arithmetic which has an upper bound less than `n`, whether in
the context of array bounds or elsewhere.

## Statements

Statements can cause side effects, so they are embedded in Haskell Monad
of type `Ivory eff a`. The `eff` type parameter is used to track some side
effects.

### Reference creation

The `local` statement creates a reference to a mutable value on the stack.
Local takes an argument of an initial value, and gives a `Ref s area`, where
type variable `s` is bound to the statement's procedure scope (discussed later)
and `area` is of Ivory kind `Area`, which is used to specify the kinds of Ivory
types which correspond to memory areas - `Stored`  atoms, such as `Sint8`,
as well as `Array`s, `Struct`s, and Refs.

### Reference use

The `deref` and `store` primitives are Ivory statements that read and write to
a given `Ref`. `deref` is polymorphic and will work on `ConstRef` types as well
- a `ConstRef` can be created from any `Ref` using the `constRef` expression -
but, of course, it is not valid to `store` into a ConstRef.

### Control Flow

Ivory does not Haskell syntax, such as the mixfix `if/then/else` or case
expressions for control flow, instead it uses a primitive `ifte_` statement,
which takes an argument of type `IBool` followed two Ivory monad arguments.

Loops are available in Ivory using the `arrayMap` primitive, which has type 
`(Ix n -> Ivory eff ()) -> Ivory eff ()`. The function `Ix n -> Ivory eff ()`
specifies a loop body which is run for each valid value of `Ix n`, which is
`0` to `n-1`. Loops may be terminated early with the `break` statement.

A nonterminating loop primitive, `forever`, is also provided by the Ivory
language. This primitive is only to be used for loops that should never
terminate, such as event loops in the implementation of an operating system
task.


## Procedures

type signature

body creates new allocation scope

call


## Structures

we use a quasiquoter to declare structures

what can be an element of a structure

the ~> operator allows you to use structure elements
in expressions, a ref to the structure becomes a ref to the
element given by the label argument


## Bit Data

embedded systems use memory mapped IO, we use special helper functions declared
using the ffi to read and write to arbitrary addresses in memory

typically these addresses in memory contain bitfields. we specify the layout of
those bitfields using a special quasiquoter syntax to generate setter/getter
labels, so we can manipulate them in a type safe way

give an example

we can specify names for individual bit patterns as well

give an example


## Modules

there is a moduledef monad, for syntax sake, that is jsut a writer for various
things you can write procs with `incl`, global memory areas with `memArea`,
dependencies with `depend` and
so on. the only thing to do with moduledef is create a `package` with it, which
gives the name of the .c/.h file that will be generated.

## External C Code interface

you can import procs, memory areas
you can include headers in a given module

