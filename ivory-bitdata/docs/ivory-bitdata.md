# Ivory Bit Data eDSL

## Introduction

The Ivory language provides additional memory and type safety compared
to C by restricting references to static and stack allocated data.
However, to implement low level hardware drivers for many peripherals,
we must be able to access memory-mapped I/O registers at arbitrary
addresses.  We describe a library built on top of Ivory for working
with I/O registers in a type safe manner, while remaining convenient
for the programmer and without sacrificing performance.

Typically, in C, memory-mapped hardware registers are accessed by reading
and writing raw integer values to address that have been cast to pointers.
Often, these values are composed of several bit fields which are set and
cleared independently.  For example:

    /* Register address and bit fields in a header file: */
    #define REG_CR1     ((volatile uint16_t *) 0x400000C0)
    #define REG_CR1_RXE 0x0020
    #define REG_CR1_TXE 0x0040

    /* Driver code in an implementation file: */
    REG_CR1 &= ~REG_CR1_RXE;
    REG_CR1 |= REG_CR1_TXE;

Programming at this level is extremely unsafe---there are no checks
that the flags are correct for the register, and no constraints on the
register's address.

The Ivory bit data library improves on this scheme by enforcing these
invariants:

* Each register has a unique type which describes its size.
* Each bit field within a register has a unique type which describes
  its size and the register it is associated with.
* Registers must exist within the I/O space of the processor, as defined
  by the memory map and defined in a machine implementation module.

## Basic Usage

This section contains a brief example use of the ivory-bitdata
library.

### Defining the Machine

First, we define the I/O spaces of our machine according to the processor
memory map:

    -- Define the IO space from our processor's memory map.
    ioArea1, ioArea2 :: IOArea
    ioArea1 = IOArea 0x40000000 0x40008000
    ioArea2 = IOArea 0x40010000 0x40015800

    -- Define and export a list of IO memory areas.
    ioAreas :: [IOArea]
    ioAreas = [ioArea1, ioArea2]

Ivory will only allow us to create registers in the address spaces
defined here.  In this case, we have two non-contiguous memory spaces.

The `IOArea` type is not exported to application code, and may only be used
within a hardware definition within the `ivory-bitdata` library itself.
This prevents the application developer from subverting the memory safety
by defining new IO areas.

### Defining Bit Data Types

Next, we define types for our registers.  The bit fields for each register
should also be typed, so we cannot modify one register according to the
fields of another.

The "bitdata" abstraction allows the programmer to define a wrapper
around a base n-bit integer type, along with a definition of how the
integer is split into fields.  It can also be used to define
enumerated types.

As an example, let's assume a hypothetical peripheral with 2 8-bit
control registers, called `CR1` and `CR2`.  The `CR1` register has
some fields used to enable the peripheral and set the baud rate, where
`CR2` contains some interrupt enable fields separated by reserved
bits:

    CR1 (8 bits):

    7                                               0
    +-----+-----+-----+-----+-----+-----+-----+-----+
    |  0  |  0  |  0  |   baud    | rxe | txe | en  |
    +-----+-----+-----+-----+-----+-----+-----+-----+

    Where baud is the 2-bit value:

    00   9600   bps
    01   19200  bps
    10   38400  bps
    11   115200 bps

    CR2 (8 bits):

    7                                                 0
    +-----+-----+-----+------+-----+-----+-----+------+
    |  0  |  0  |  0  | rxie |  0  |  0  |  0  | txie |
    +-----+-----+-----+------+-----+-----+-----+------+

We can define the layout of these registers and an enumerated type for
the baud rate using the bitdata quasiquoter:

    [bitdata|
      bitdata Baud :: Bits 2
        = baud_9600   as 0
        | baud_19200  as 1
        | baud_38400  as 2
        | baud_115200 as 3

      bitdata CR1 :: Bits 8 = cr1
        { cr1_baud :: Baud
        , cr1_rxe  :: Bit
        , cr1_txe  :: Bit
        , cr1_en   :: Bit
        } as 3b0 # cr1_baud # cr1_rxe # cr1_txe # cr1_en

      bitdata CR2 :: Bits 8 = cr2
        { cr2_rxie :: Bit
        , cr2_txie :: Bit
        } as 3b0 # cr2_rxie # 3b0 # cr2_txie
    |]

(Because of Template Haskell restrictions, you cannot define and use
bitdata definitions in the same module like this.  In this example,
the "Baud" definition would need to be defined as a separate module.)

These definitions are very similar to the usual Haskell syntax for
defining data.  We can define sum types, with multiple constructors,
each of which can contain fields within braces.

One difference from a Haskell data definition is the "`as`" clause
used to define a bit data layout for each constructor.  This layout
defines the order and position of each field within the data created
by that constructor.  It is also used to include constant data, such
as bits that are always 1 within a register.  If a layout is present
but contains only constants, then it is effectively an enumerated
type, such as "`Baud`" above.

If there is no layout clause, a default clause is used, which is
simply each field in the order it was declared, starting with the
most significant bit.  Any bits left over (or the default layout for a
constructor will no fields) will be padded with zeros.

For each bit data definition, the quasiquoter generates:

* An abstract type for the bit data type.
* An instance of the `BitData` type class for the bit data type.
* A function for each bit data constructor.
* A field definition for the fields of each constructor.

### Defining Registers

Next, we define one or more concrete instantiations of these registers
at specific memory addresses.  The memory addresses must lie within the
I/O memory spaces defined in the machine description.  The `BitDataReg`
type parameter encodes the bit data type contained within that register:

    regCR1 :: BitDataReg CR1
    regCR1 = mkBitDataReg 0x40013000

    regCR2 :: BitDataReg CR2
    regCR2 = mkBitDataReg 0x40013008

The library provides the `getReg` and `putReg` functions to read and write
these registers, treating the value inside as the associated bit data type:

    getReg regCR1 :: Ivory eff CR1
    putReg regCR1 :: CR1 -> Ivory eff ()

### Atomic Register Updates

Because writes to a hardware register have an immediate effect on a
device, we often need to modify several fields atomically.  The bit
data library provides the `setReg` and `modifyReg` functions that
accept a monadic sequence of bit operations to perform.  Typically,
this will be an inline Haskell "do" block.  From this block, a single
C expression is generated which sets and clears fields in a single
write:

    setReg    regCR1 :: BitDataM CR1 a -> Ivory eff a
    modifyReg regCR1 :: BitDataM CR1 a -> Ivory eff a

The `setReg` function first clears the register to zero, then applies
its argument.  The `modifyReg` function does not clear the register
first.

The `BitDataM` monad implements a very restricted imperative
sublanguage for modifying the bits of a register:

    setBit   :: (BitData d) => BitDataField d Bit -> BitDataM d ()
    clearBit :: (BitData d) => BitDataField d Bit -> BitDataM d ()
    setField :: (BitData d, BitData b)
             => BitDataField d b -> b -> BitDataM d ()

For example, a driver initialization function may need to set some bits
and clear others:

    driver_init :: Def ('[] :-> [])
    driver_init = proc "driver_init" $ body $ do
      modifyReg regCR1 $ do
        setBit   cr1_rxe
        clearBit cr1_txe
        setField cr1_baud baud_38400
        setBit   cr1_en

## Implementation

### Design

The design of the Ivory bitdata library is influenced by several key
features of the Haskell type system.

* The GHC quasiquotation extension can be used to add bit data to the
  language using a natural syntax that looks much like a Haskell data
  type definition.

* The quasiquoter generates regular Ivory code---it does not use any
  unexported functions or modules to get around Ivory's memory safety
  restrictions.  The only non pure-Ivory part of the implementation
  are the primitives defined in C to read and write hardware
  registers.

* Each bit data type is a distinct Haskell type.  This type is a thin
  wrapper over the underlying integer type, but it must be explicitly
  converted.

* Bit data types are defined in terms of "bit types", such as `Bits
  16`, which use the GHC `DataKinds` extension to obtain type-level
  natural numbers.  The quasiquoter consults the type environment to
  convert types like `Bits 16` to their representation type, such as
  `Uint16`.

* Each field of a bit data type is defined as an accessor mapping the
  container type to the field type.  The bit data type and field types
  must "line up" on field access.

### Bits Type

The abstract type "`Bits n`" describes an `n` bit unsigned integer.
The type parameter `n` is a type-level natural number, using the
`DataKinds` extension in GHC.

    data Bits (n :: Nat)

As a convenient shorthand, a single bit type is defined as:

    type Bit = Bits 1

Since the base language and machine do not support integers of
arbitrary bit sizes, we must map the "`Bits n`" type to a
representation that is supported.  The type function "`BitRep n`"
returns a suitable type to hold an unsigned integer of at least `n`
bits.

"`BitRep n`" is defined as the smallest of `Uint8`, `Uint16`,
`Uint32`, or `Uint64` that can hold `n` bits.  If `n` is larger than
64, no representation type is defined:

    type family BitRep (n :: Nat) :: *
    type instance BitRep 1 = Uint8
    ...
    type instance BitRep 8 = Uint8
    type instance BitRep 9 = Uint16
    ...
    type instance BitRep 16 = Uint16
    type instance BitRep 17 = Uint32
    ...
    type instance BitRep 32 = Uint32
    type instance BitRep 33 = Uint64
    ...
    type instance BitRep 64 = Uint64

Under the hood, the `Bits` type is simply a wrapper around the
underlying representation type:

    newtype Bits (n :: Nat) = Bits (BitRep n)

The following functions are defined to convert bit values to and
from their representations:

    bitsToRep :: Bits n -> BitRep n
    repToBits :: BitRep n -> Bits n

The `repToBits` function is not total---if `n` is not equal to the bit
size of the representation type there are many input values that do
not fit in the bit type result.  The current implementation of
`repToBits` masks off any bits that are out of range.

There are other error handling strategies, such as returning "`Maybe
(Bits n)`" or having the caller supply a default value or continuation
for the error case.  We have chosen the masking strategy because sum
types are not currently supported in Ivory.  A default value or
continuation could be used, but it is difficult for the programmer to
supply a meaningful default value or error handling function.  This is
left as an area for further design exploration.

In some situations, it may be desirable to have a full set of numeric
operations available on "`Bits n`" values.  However, typing some of
these operations requires type-level arithmetic that is not supported
in released versions of GHC at the time of this implementation.  Since
our primary concern is to implement type-safe hardware register
access, we have opted to not define these operations on raw bit
vectors, relying instead on the higher-level abstraction of bit data
definitions.

### Bit Literal Syntax

When defining bit data, it is useful to have a literal syntax for
natural numbers with a known size in bits.  We generalize the C
language syntax for hexadecimal numbers to support literals of both
unknown and known size.

The "bitdata" quasiquoter contains a parser for this bit literal
syntax:

    bitliteral = bitlit_unknown
               | bitlit_known

    -- Unprefixed numbers have an unknown size and are
    -- assumed in base 10.
    bitlit_unknown = [0-9]+

    -- Prefixed numbers specify their size and base.  If "size"
    -- is zero, interpret it as an unknown size literal in the
    -- specified base.
    bitlit_known = size value
    size         = [0-9]+
    value        = [Bb] [0-1]+
                 | [Dd] [0-9]+
                 | [Xx] [0-9A-Fa-f]+

#### Examples

    16d0     -- decimal 0, 16 bits
    8xFF     -- hex FF, 8 bits
    6b110011 -- binary 110011, 6 bits
    100      -- decimal 100, unspecified size
    0x1234   -- hex 1234, unspecified size

#### Expansion

The "bitdata" expression quasiquoter converts bit literal syntax to
values of type "`Bits n`", where `n` is defined as the bit size for
known bit literals, and left polymorphic for literals of unspecified
size.

Since the parser statically checks the value against the maximum value
when the size is known, it can use an unsafe internal primitive to
construct the value without bounds checking.  (The `unsafeIntToBits`
primitive is not exported, but for brevity it is written here as an
unqualified symbol).

For unknown size literals, we must use the general `repToBits`
function.

For example:

    [bitdata| 16d0 |] --> unsafeIntToBits 0 :: Bits 16
    [bitdata| 8xFF |] --> unsafeIntToBits 255 :: Bits 8
    [bitdata| 100  |] --> repToBits (fromIntegral 100)

The (simplified) type of this last expression is:

    [bitdata| 100 |] :: SingI n => Bits n

### Quasiquoter

The "bitdata" declaration quasiquoter converts a bit data definition
to type and field definitions.  The quasiquoter is implemented in
several stages:

* A Parsec parser converts the input string to a simple AST, using
  `String`s for identifier names and a simple ADT `BitTy` for types.

* This AST is annotated by looking up identifiers and types.  `String`
  is converted to `Name` and `BitTy` is converted to `Type`.  We also
  look up the size of types in bits by using `reifyInstances` to
  "call" type functions at compile time.

* Bit data layouts are processed, calculating the bit position and
  length of each data field.

* The Haskell code splice is generated using Template Haskell.  We
  generate a `newtype` definition for the bit data type, an instance
  of the `BitData` type class, a constructor function, and a
  `BitDataField a b` value for each data field.

### BitData Type Class

Each bit data type must provide an instance of the `BitData` type
class.  This class provides functions to convert a bit data type to
and from a raw `Bits n` type:

    toBits   :: BitData a => a -> BitType a
    fromBits :: BitData a => BitType a -> a

The only built-in instance of `BitData` is for `Bits n`, which
implements `toBits` and `fromBits` as the identity function.

Like `repToBits`, `fromBits` masks out any upper bits that are out of
range of the value.

`fromBits` also allows the creation of so-called "junk" values, by
converting bit patterns that cannot be created using the generated
constructor.  In the current implementation, we allow these junk
values as in (ref Iavor's paper).

The bitdata library also defines functions for converting bit data to
its representation and back:

    toRep   :: BitData a => a -> BitDataRep a
    fromRep :: BitData a => BitDataRep a -> a

Where "`BitDataRep`" is a type function that converts a bit data type,
like `CR1` to its representation type `Uint8`.

### Constructor

The quasiquoter also generates a constructor function for each
constructor defined in a bit data definition.  In our example, we
obtain a single constructor for each register type:

    cr1 :: Baud -> Bit -> Bit -> Bit -> CR1
    cr2 :: Bit -> Bit -> CR2

These constructors use the layout clause to assemble the components
into the right order, inserting any constant data as required.

### Bit Fields

For each field defining in a bitdata definition, the quasiquoter
generates a `BitDataField` definition.  This describes the position
and length of the bit field, and associates it with its bit data type.

In our example, the fields of CR1 are generates with these types:

    cr1_baud :: BitDataField CR1 Baud
    cr1_rxe  :: BitDataField CR1 Bit
    cr1_txe  :: BitDataField CR1 Bit
    cr1_en   :: BitDataField CR1 Bit

Similar to the concept of a "lens", these fields indicate that, given
a value of type "CR1", we can get or set a value of type "Baud" using
the "cr1_baud" field.  The following functions implement getting and
setting fields as pure functions:

    getBitDataField :: (BitData a, BitData b)
                    => BitDataField a b -> a -> b
    setBitDataField :: (BitData a, BitData b)
                    => BitDataField a b -> a -> b -> a

Because the types of the field and parameters must match, this catches
any mismatch of registers and bit fields at compile time:

    -- register/field mismatch!
    get_txie :: CR1 -> Bit
    get_txie x = getBitDataField x cr2_txie

## Related Work

This library is a simplified implementation of the bit data
abstraction described in
[High-level Views on Low-level Representations (Diatchki, Jones, and Leslie)][1].

[1]: http://yav.purely-functional.net/publications/bitdata.pdf

## Summary and Future Work

The Ivory bit data library has been used to generate C drivers for
several internal peripherals of an STM32F4 microcontroller.

There are several interesting areas to explore for future work:

* Implementation of more error handling strategies for partial
  conversions from bit data types to their representations.  This
  requires thinking about how to handle "junk" data, and depends on
  the error handling strategy used by the DSL being used.

* Implement more operations on bit types such as `Bits 16`.
  Currently, the "type-nats" branch of GHC contains a solver for
  type-level natural numbers that would allow us to type functions
  such as `bitConcat :: Bits a -> Bits b -> Bits (a + b)`, which
  cannot be done in the current implementation.

* Add a `case` function to the quasiquoter for bit data sum types.
  This function would use the constant bits defined in the bit data
  layout to distinguish between multiple constructors, and call an
  appropriate function with the constructor fields.

* Currently, the library has a direct dependency on the Ivory DSL.
  However, it should be possible to abstract out the types being used
  by the bitdata library into a type class and generalize it to
  other DSLs (and native Haskell via `Data.Bits`).

* Manually writing bit data definitions from a specification document
  is a potentially error-prone process.  We could reduce this risk
  significantly if we could generate bit data definitions directly
  from machine-readable specifications supplied by a chipset vendor.
