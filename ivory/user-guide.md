% Ivory User Guide


Introduction
================================================================================
Ivory is an embedded domain specific language that tries to eliminate certain
classes of bugs from the C programming language.


Language Features
--------------------------------------------------------------------------------

### Memory Safety
Ivory removes the possibility for accidental indexing out of array bounds, and
null pointer dereferencing statically.  All allocation in Ivory is done via a
special primitive that allows only stack allocation, and requires that
everything allocated gets a zero-initializer.  One exception to this is that
global data allocated is given no initializer, placing the resulting definitions
in `.bss`.


### C Integration
Ivory makes it easy to work with existing C source.  Function symbols and even
structures can be imported into an Ivory program, and as code for Ivory is
generated via C, calling Ivory from C is straight forward.


Kinds
================================================================================

There are three kinds in Ivory:

 * `*` (star)
 * `Proc`
 * `Area`

`*` (valued types)
--------------------------------------------------------------------------------

The `*` kind represents types that have values, which in ivory includes numeric
types, booleans, characters, pointers, function pointers and references.

`Proc` (functions)
--------------------------------------------------------------------------------

Functions defined in Ivory have their own kind, `Proc`.  The purpose for this
distinction is to formalize the fact that Ivory does not support higher-order
functions.  There is an escape: the `ProcPtr` type represents a pointer to a
function, allowing it to be invoked through an `indirect` call.  The syntax for
constructing a type of kind `Proc` is the function arrow:

 * `'[] :-> ()` - a function that takes no arguments and returns no value
 * `'[Uint32] :-> Uint8` - a function that takes one `Uint32` and returns a
   `Uint8` value.

`Area` (memory areas)
--------------------------------------------------------------------------------

Memory areas, inspired by "Strongly Typed Memory Areas" [1], represent the types
of aggregate structures that take up a region of memory.  The supported
aggregate structure types are:

 * `Stored :: * -> Area` - The most simple memory area, representing a single
   stored value.
 * `Struct :: Symbol -> Area` - An aggregate structure, named by the type-level
   string that is its argument.
 * `Array :: Nat -> Area -> Area` - An array of `n` areas.
 * `CArray :: Area -> Area` - The same as an Array type, but without the
   type-level length parameter.  This is suitable for interfacing with a
   function implemented in C.

Types that have kind `Area` are never meant to have their values used directly,
instead they will be manipulated through the use of a `Ref` or `Ptr`.


Language
================================================================================

`Ivory r a` (statement monad)
--------------------------------------------------------------------------------

All statements in Ivory are performed in the context of the `Ivory` monad.  The
monad has two parameters, `r` and `a`, which represent the return type of the
enclosing block, and the return type of the statement respectively.  For
example, calling a function that produces an `IBool` value in isolation will
produce something of type `Ivory r IBool`, while the statement that returns a
value used with something of the type `IBool` will have the type `Ivory IBool
()`.


`()` (void)
--------------------------------------------------------------------------------
There is one special type in Ivory, named `()`.  This corresponds to the `void`
type in C, in that it's a valid return type.  Where it differs from C is that it
is *only* valid as a return type; void pointers are not allowed.


`IBool`
--------------------------------------------------------------------------------
`IBool` is the type of boolean values in Ivory.  For producing values of type
`IBool`, there are two classes, `IvoryEq` and `IvoryOrd`.  Additionally, there
are two constants of type `IBool`, `true` and `false`.


### `IvoryEq`
`IvoryEq` provides two functions for testing equality, `(==?)` and `(/=?)`.
They mirror the Haskell equality testing functions, though they have a question
mark appended to avoid conflicts.


### `IvoryOrd`
`IvoryOrd` provides four functions for testing the order of two elements,
`(>?)`, `(>=?)`, `(<?)` and `(<=?)`.  Again, they mirror the members of the
Haskell `Ord` class, with a question mark appended to avoid conflicts.


### Consuming `IBool` values
There are two ways to consume a value of type `IBool`, an if-then-else
statement, or a conditional expression.  If-then-else is implemented by the
`ifte` function, which takes three arguments: an expression of type `IBool`, a
branch for the `true` case, and a branch for the `false` case.

```haskell
ifte :: IBool -> Ivory r a -> Ivory r b -> Ivory r ()
```

Conditional expressions mirror the ternary conditional operator of C:

```haskell
(?) :: IBool -> (a,a) -> a
```


Numeric Types
--------------------------------------------------------------------------------
All Numeric types in Ivory support operations from the Haskell `Num` class.
Additionally, the floating-point types support the use of the `Floating` and
`Fractional` classes.  All numeric types support comparison and equality testing
via the use of the `IvoryEq` and `IvoryOrd` classes.


Integral Types
--------------------------------------------------------------------------------


### `IvoryIntegral`
This type class provides the `div` and `mod` operations.


### `Uint8`, `Uint16`, `Uint32`, and `Uint64`
Ivory provides a set of unsigned integer types, `Uint8`, `Uint16`, `Uint32`, and
`Uint64`.  These types are bindings to the C types `uint8_t`, `uint16_t`,
`uint32_t` and `uint64_t` respectively.  These types also have an instance for
`IvoryIntegral`.


### `Sint8`, `Sint16`, `Sint32`, and `Sint64`
Ivory provides a set of signed integer types, `Sint8`, `Sint16`, `Sint32`, and
`Sint64`.  These types are bindings to the C types `int8_t`, `int16_t`,
`int32_t` and `int64_t` respectively.  These types also have an instance for
`IvoryIntegral`.


Floating-point Types
--------------------------------------------------------------------------------

### `IFloat` and `IDouble`
Ivory provides two types for floating-point numbers, `IFloat` and `IDouble`.
These correspond to the `float` and `double` types present in C.


### Floating-point Conversion
When writing code that works with floating point numbers, it can become
necessary to convert to and from `IFloat` or `IDouble` from many of the integral
types.  For this purpose, the `IvoryFloatCast` class exists.  It provides two
functions: `toFloat` for converting from an integral type to a floating-point
type, and `fromFloat` for converting back, with a default in the case of a
`NaN`.


Structures
--------------------------------------------------------------------------------


### Quasi-quoter

### Labels

### Externally defined memory areas


`Ref area` (references)
--------------------------------------------------------------------------------

References in Ivory are pointers that are guaranteed to point to something.
Having one around means that dereferencing and storing can be done without
requiring a check for null.


### Ref Allocation

__Section needs to be revised__

References are allocated in a way that requires them to alway have memory that
backs them.  This is currently implemented by only allowing stack allocation.
Stack allocation is performed through the `local` primitive, which will create a
`Ref` that points to a zero-initialized chunk of memory.  The `local` primitive
takes no arguments, instead using type inference to figure out what the
allocated structure is from the way that it is used.  In this example, local is
able to figure out, through type inference, that a `Ref (Stored IBool)` is being
allocated.

```haskell
  do ...
     r <- local
     store r true
     b <- deref r
     ...
```

As Ivory does not currently guard against returning references allocated with
local from their enclosing context, it's the programmer that has to guard
against that case.


### `deref` / `store`
There are only two operations defined for working
with the values that references contain:

 * `deref :: Ref (Stored a) -> Ivory r a` - read the value out of a reference
 * `store :: Ref (Stored a) -> a -> Ivor r ()` - store a value into a reference

Notice that these two operations only work on references that point to a
`Stored` thing, this implies that you can only `deref` and `store` things that
are of kind `*`; working with references that contain things of type `Struct` or
`Array` require the user to index down to something that is `Stored`.


### Indexing
As `deref` and `store` can only be used with references that point to a thing of
type `Stored a`, there needs to be ways of turning references to a aggregate
type like a `Struct` or an `Array`.  To this end, two functions exist:

```haskell
(~>) :: Ref (Struct struct) -> Label struct area -> Ref area
(!) :: Ref (Array len area) -> Ix rep len -> Ref area
```

The `(~>)` operator will turn a reference to a `Struct` into a reference to one
of its fields, while the `(!)` operator will index into an array, producing a
reference to one of its elements.  Both of these operations are pure, as using
them corresponds calculating a new address safely, from a known address.


### The `Ix` type
The `(!)` operator takes as the `Array` index something of type `Ix rep len`.
This type is described in more detail in [1], however it's sufficient to say
that it is an index value that lies within the range `[0,len)`.  This then
ensures that references that point to an array cannot be indexed out of bounds,
as you can only use an index that is statically known to fall within the range
of its elements.

One difference between our implementation and that of [1] is that we include a
`rep` parameter to the `Ix` type, which indicates what the underlying
representation for the index will be.  It's worth noting that this opens the
door to problems in which a representation that is too small is chosen for an
array, however these sorts of problems can be ruled out statically once an
arithmetic solver is integrated into the GHC type system.


`Ptr area` (pointers)
--------------------------------------------------------------------------------
Pointers in Ivory can not be manipulated directly.  Following the treatment in
[1], they represent something that can be thought of as `Maybe (Ref area)`.
Because of this distinction, pointers must first be unwrapped, and then used as
a reference in context.  To perform this unwrapping, the `withRef` operation is
provided.

```haskell
withRef :: Ptr area -> (Ref area -> Ivory r t) -> Ivory r f -> Ivory r ()
```

Under the hood, this is implemented as a null-check, using the true continuation
when the pointer is not null, and falling back on the false continuation when it
is.


Functions
--------------------------------------------------------------------------------
Function symbols that are defined at the top level have the type `Def proc`
where `proc` is something of kind `Proc` representing the type signature for the
function.  These symbols are suitable for use with the `call` and `call_`
functions.


### Calling Functions
Functions can be called in two ways: `call` or `call_`.  Conceptually, they take
something of the type `Def proc`, then produce a Haskell function that will
consume arguments in the way that the function being called would.  They differ
in that `call_` will always return `()`, never naming its result.

As an example, consider the function `f` defined below, combined with the two
variants of call.

```haskell
f :: Def ('[Uint32] :-> Uint32)
call  f :: Uint32 -> Ivory r Uint32
call_ f :: Uint32 -> Ivory r ()
```

### Function Pointers
It is possible to turn a defined function into a function pointer that can be
passed as an argument to other functions.  This is done via the `procPtr`
function:

```haskell
procPtr :: Def proc -> ProcPtr sig
```

As the result of a call to `procPtr` is something of the type `ProcPtr`, using
it with the `call` functions will produce a type error.  To solve this problem,
there are two variants on the `call` functions named `indirect`.  They behave
the same way, but can only be used with `ProcPtr proc` typed symbols.


### Function Definition
Functions can be introduced in three ways: importing from a C header, defining
an `extern` symbol, and providing a function body.  Each definition will produce
a top-level value of type `Def proc`, where `proc` is the type of the symbol.

When importing a function from a C header, the `importProc` function is used.
It takes the name of the symbol to be imported, as well as the name of the
header to include as its arguments.  Functions imported this way are required to
be given a type signature, which will inform the call functions of the
argument types to that function.

Importing external symbols with the `externProc` function is similar to
importing a function from a C header, though there is no need to specify a
header path.  Again, a signature is required.

Defining functions with a body is done with the `proc` function.  The `proc`
function takes two arguments, the name for the function in the outputted code,
and a function that describes its behavior.  For example, the body of the
function symbol `f` defined below is typed separately from the definition, to
illustrate the connection between the body definition and the signature.  Note
that the return type of the `f_body` function is specified as the `r` parameter
to the `Ivory` monad type, and that the monadic return type is set as `()`.

```haskell
f :: Def ('[Uint32] :-> Uint32)
f  = proc "f" f_body

f_body :: Uint32 -> Ivory Uint32 ()
f_body val = ret val
```


### Returning values
In order to return a value from a function body, either the `ret` or `retVoid`
functions must be used.  The two functions have the following type:

```haskell
ret     :: r -> Ivory r ()
retVoid :: Ivory () ()
```

Note that these two functions are the only way to affect the `r` parameter of
the `Ivory` monad.  The purpose of this is that if multiple uses of `ret` are
present in a function body, having them fix this `r` type will cause them to all
have the same type.  As an example, consider this fragment, which will cause a
type error:

```haskell
do ...
   ret true
   ...
   retVoid
```


Module Definition
--------------------------------------------------------------------------------
Ivory modules are defined separately from the Haskell module system.  The
purpose they serve is that of a compilation unit; everything added to an Ivory
module will be included in the resulting C source and header.  Modules can also
be imported into the interpreter, loading the described environment into the
current evaluation context.  Additionally, to facilitate code reuse, modules
are allowed add dependencies on each other.


### Module Definitions
Modules are defined with the `package` function, which takes a `String` for the
name of the generated module, and a `ModuleM` value.  `ModuleM` values are
monadic actions that describe what goes in to a module definition.

### Exporting Functions
Functions, whether imported from C or defined in Ivory, are exported with the
`incl` function.  Using `incl` will different things in different contexts:

 * For an imported function, incl will write out an `#include` directive for the
   header that defines the symbol
 * For an extern function, incl will write out an extern definition in the
   generated header
 * For a function defined in Ivory, it will write the function prototype in the
   header, and the function body in the source

Functions, memory areas and structs can be made private -- meaning that their
declarations are only put in the source file, not the header. This is done by
using using a `private` block in the module definiton as shown below. As shown,
one can also explicitly use a `public` block -- but public visibility is the
default.

```haskell
cmodule ∷ Module
cmodule = package "sample" $ do
  private $ do
    defStruct (Proxy ∷ Proxy "Foo")
    defMemArea privateFoo
    incl privateHelper1
    incl privateHelper2
  public $ do
    defStruct (Proxy ∷ Proxy "Bar")
    incl publicFunction
```


References
================================================================================

[1] "Strongly Typed Memory Areas"
  <http://web.cecs.pdx.edu/~mpj/pubs/bytedata.html>
