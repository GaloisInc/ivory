# Ivory Tutorial at TARDEC

## Environmental Setup

Goal: confirm that VM is properly configured, and that all necessary
tools and code are available.

- Verify stack installation
- Check out the Ivory repository
- Use stack to configure an Ivory build environment

### Installing stack

Stack is a tool for managing Haskell packages. We're going to use it
to build all of the libraries in the Ivory repository, and then our
own example applications. To make sure stack is installed, run:

```sh
$ stack --version
Version 1.2.0 x86_64 hpack-0.14.0
```

If this does not work, install stack by following the directions for
your OS at the [stack README][stack-install].

[stack-install]: http://docs.haskellstack.org/en/stable/README/#how-to-install

### Check out the Ivory repository

Your VM should have the directory `smaccmpilot-build` which contains
the `ivory` repository along with the other packages we use to build
SMACCMPilot. If you cannot find this directory, you can check out the
Ivory repository directly:

```sh
$ git clone https://github.com/galoisinc/ivory
$ git checkout tardec-tutorial
```

If you do not have git, click the `download zip` button on the
[Ivory github page][ivory-github], and unzip the archive.

[ivory-github]: https://github.com/galoisinc/ivory/tree/tardec-tutorial

### Build with documentation

Using a terminal, change directory to the checked out or unzipped
Ivory repository, then use stack to set up your environment:

```sh
$ stack setup
$ stack install hscolour
```

You will now have the correct Haskell compiler and tools required to
build Ivory and its documentation:

```
$ stack build --haddock
```

### Browse the documentation

Once the stack build finishes, and it may take a while, it will print the
location of three documentation indexes: one for local packages, one for local
packages and dependencies, and one for just dependencies. The index for local
packages will contain documentation for the Ivory packages only, and will be the
most helpful when writing Ivory programs.

The location printed will look something like this, with `$ivory_repo`, and
`$arch` being something specific to your system:

```
$ivory_repo/.stack-work/install/$arch/lts-6.10/7.10.3/doc/index.html
```

## The example program skeleton

Open the `ivory-tutorial/Example.hs` file in your text-editor of choice. It has
some boilerplate filled out for you already, which we'll walk through.

### The `exampleModule` definition

Ivory calls a compilation unit a module. Modules are defined by using the
`package` function, which packages a collection of named declarations within a
single module. You can see this in effect at line 16, where the `ivoryMain`
function is included in `exampleModule` by using the `incl` statement.

### The `ivoryMain` procedure

Programs in Ivory are a collection of procedures. The `ivoryMain`
procedure is introduced in lines 18-22, as a procedure that simply
returns `0`.  It also has a type signature that reflects this fact --
`ivoryMain` accepts no formal parameters, denoted by the `'[]` (empty
list) to the left of the `':->` (arrow), and returns a value of type
`Sint32`. The body of the `ivoryMain` procedure currently only
includes a single statement, `ret 0`, that causes it to return
immediately. As the tutorial progresses, we'll make the code do
something a bit more interesting.

### A note on Haskell syntax

You may have noticed that in the type signature for `ivoryMain` that
some portions of the signature have a leading single-quote. This is
due to the `DataKinds` language extension that allows Ivory to have
more precise types. For the most part you won't need to worry about
this feature, other than needing to include the pragma in your Ivory
modules.  The Haskell compiler will remind you when you've forgotten
to use the leading single-quote.

The `$` operator is a common sight in many Haskell programs, including
Ivory. While it may look strange, `$` is just syntactic sugar for
function application. Because of its associativity, Haskell programs
often use `$` in place of pairs of parentheses. For example, without
`$`, `ivoryMain` would look like:

```haskell
ivoryMain  =
  proc "main"
  (body
    (do ret 0))
```

### The `main` function in `ivory-tutorial/codegen.hs`

The `main` function in `codegen.hs` is the entry-point to the Ivory
code generator. The `compile` function takes a list of Ivory modules
to compile, and a list of other artifacts (such as configuration
files) to bundle in the final product. As we're only building one
Ivory module, we give a list of a single element as the first
argument, and as there are no additional artifacts that we would like
bundled, we give an empty list as the second. You can learn more about
other ways to invoke the C code generator in the documentation for the
`ivory-backend-c` package.

## Building the C module

At this point, even though our `main` doesn't do anything interesting
yet, let's generate and compile the module. This will let us get the
feel for the tools and make sure that everything is working. Run the
`codegen.hs` module, giving it an output directory, and it will output
a few C headers and a single C source file to the output directory.

```sh
$ stack codegen.hs --src-dir=out
$ ls -F out
example.c
example.h
ivory.h
ivory_asserts.h
ivory_templates.h
```

You can also try `stack codegen.hs --help` to see all of the Ivory
compiler options.

Now, we can build the example module by using gcc (or your preferred C
compiler). Make has a built-in rule that can do everything for us and
saves a bit of typing:

```sh
$ make -C out example
cc     example.c   -o example
$ ./out/example
$ echo $?
0
```

Running `example` doesn't do anything interesting yet, but it does
give us the correct exit code, `0`.


## Game character example

Let's make a character manager for a simple role-playing game. To
begin, we'll add the basic stats about our character, number of health
points and number of magic points.

### Defining a structure

We'll start by defining a `struct` to store the stats. Add this
definition to `Example.hs`:

```haskell
[ivory|

struct Character
  { uint16_t hp
  ; uint16_t max_hp
  ; uint16_t mp
  ; uint16_t max_mp
  }

|]

```

This will be enough information to keep track of how much health
points and magic points our character has. To make sure that it will
be included in the generated module, add the following line after the
`incl ivoryMain` line in the definition of `exampleModule`:

```haskel
       defStruct (Proxy :: Proxy "Character")
```

Haskell is indentation sensitive, so make sure that the `d` lines up
just under the `i` of `incl` on the previous line.

Just to confirm that it works, run `stack codegen.hs --std-out`, and
see that the `struct` declaration has made it into the output.

### Defining a new function

Next, we'll need functions to interact with `Character` values. Let's
start by adding a few utility functions for manipulating health and
magic points:

```haskell
heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
    do current_hp <- deref (ref ~> hp)
       total_hp   <- deref (ref ~> max_hp)
       new        <- assign (current_hp + amount)
       store (ref ~> hp) ((new >? total_hp) ? (total_hp,new))
```

There is quite a bit new here, so let's take this definition a line at
a time:

#### Type signatures

```haskell
heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
```

This is the type signature of the `heal_char` definition, indicated by
the `Def` type constructor. Compare this signature to that of
`ivoryMain`:

```haskell
ivoryMain :: Def ('[] ':-> Sint32)
```

The `ivoryMain` type can be read as a function that takes no arguments
(`'[]`) and returns a signed 32-bit integer (`Sint32`). This
corresponds to a familiar declaration in C:

```c
int32_t main(void)
```

In fact, this is exactly the signature that Ivory has generated in
`out/example.h`.

For `heal_char`, we instead have a function that takes two arguments,
an unsigned 16-bit integer (`Uint16`) and a reference to a `Character`
struct (`Ref s ('Struct "Character")`), and returns nothing
(`()`). This corresponds to the following C declaration:

```c
void heal_char(uint16_t amount, struct Character *ref);
```

#### Procedure declarations

```haskell
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
```

Here we define what the procedure will be called in C (`proc
"heal_char"`), and give names to its arguments so that we can use them
in the body of the procedure (`\ amount ref`). Then, we tell Ivory
that we are ready to define the body of the procedure (`body`).

#### Procedure bodies

The bodies of most Ivory procedures begin with a `do`, which is the
Haskell keyword that introduces a block of sequential code.

```haskell
    do current_hp <- deref (ref ~> hp)
       total_hp   <- deref (ref ~> max_hp)
```

Here we dereference the `hp` and `max_hp` fields from the `Character`
struct reference (`ref`), and then assign those value to `current_hp`
and `total_hp`. In C, this might look like:

```c
    uint16_t current_hp = ref->hp;
    uint16_t total_hp = ref->max_hp;	
```

In addition to dereferencing fields, we can also introduce local
variables with `assign`:

```haskell
       new        <- assign (current_hp + amount)
```

Finally, we store the new health points value back in the `Character`
struct, first checking to make sure we don't exceed the maximum health
points value:

```haskell
       store (ref ~> hp) ((new >? total_hp) ? (total_hp,new))
```

The `?` operator in Ivory works much like the ternary operator in C,
so this line might look like:

```c
    ref->hp = (new > total_hp) ? total_hp : new;
```

Finally, notice that we don't use `ret` in this procedure, as we did
in `ivoryMain`. In Haskell and Ivory, the value of the last line of a
`do` block is used as the final value of the whole block. Since this
procedure is meant to return nothing (`()`), and `store` returns
nothing, this works out.

### Abstracting similar procedures

Let's add a procedure to increase the character's magic points, much
like we did with health points:

```haskell
recover_mp :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
recover_mp  =
  proc "recover_mp" $ \ amount ref ->
  body $
    do current_mp <- deref (ref ~> mp)
       total_mp   <- deref (ref ~> max_mp)
       new        <- assign (current_mp + amount)
       store (ref ~> mp) ((new >? total_mp) ? (total_mp,new))
```


Make sure to add `incl heal_char` and `incl recover_mp` to the
definition of `exampleModule`. Now, running `stack codegen.hs
--std-out` should show the two procedures. Examining the output next
to the original source program, you'll see that the code for
`heal_char` and `recover_mp` look quite similar. It would be nice to
abstract the operation of adding a value to a field, which would save
us typing but also reduce the possibility that bugs will creep in.

In Ivory, we have two options for doing this: we could just make a
helper procedure to be generated in C, and call it from the C code in
both `heal_char` and `recover_mp`.

Alternately, since Ivory is hosted in Haskell, a fully general-purpose
programming language, we can use Haskell as a macro language for
Ivory, defining the behavior once and inlining it into the definition
of both procedures.

#### A helper procedure in C

Let's have a look at the first approach, which is how this abstraction
would look in typical C code:

```haskell
add_var :: Def ('[Uint16, Ref s ('Stored Uint16), Ref s' ('Stored Uint16)] ':-> ())
add_var  =
  proc "add_var" $ \ amount var max_var ->
  body $
    do current <- deref var
       total   <- deref max_var
       new     <- assign (current + amount)
       store var ((new >? total) ? (total,new))

heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
    call_ add_var amount (ref ~> hp) (ref ~> max_hp)

recover_mp :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
recover_mp  =
  proc "recover_mp" $ \ amount ref ->
  body $
    call_ add_var amount (ref ~> mp) (ref ~> max_mp)
```

Make sure to add `incl add_var` to the definition of `exampleModule`
so that the new function will be included in the generated module.  In
this version, the implementations of `heal_char` and `recover_mp`
become calls to `add_var`, passing along the relevant references to
`hp` or `mp`.

If you again run `stack codegen.hs --std-out`, you will see that
`heal_char` and `recover_mp` have become one-line wrapper functions,
and `add_var` is doing the real work.

#### A macro for building procedures

Let's contrast the helper procedure approach with the macro version:

```haskell
add_var :: Uint16 -> Ref s ('Stored Uint16) -> Ref s ('Stored Uint16) -> Ivory eff ()
add_var amount var max_var =
  do current <- deref var
     total   <- deref max_var
     new     <- assign (current + amount)
     store var ((new >? total) ? (total,new))

heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
    add_var amount (ref ~> hp) (ref ~> max_hp)

recover_mp :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
recover_mp  =
  proc "recover_mp" $ \ amount ref ->
  body $
    add_var amount (ref ~> mp) (ref ~> max_mp)
```

Be sure to *remove* the `incl add_var` from the `exampleModule`
definition. In this version, `add_var` has been turned into a normal
Haskell function that produces results in the `Ivory` monad, rather
than directly producing an Ivory `Def`. Note that we also no longer
need to use `call_` when using `add_var`. When you generate the code
again, you will see that the implementation of `heal_char` and
`recover_mp` look exactly the same as when we defined them fully on
their own, but we've factored out the implementation into one place so
that we don't expose ourselves to the same sorts of copy-paste bugs
that existed before.

### Using C functions

Let's continue using the macro version, and write a simple
game scenario in the `ivoryMain` procedure. To test the `heal_char`
functionality, we'll simulate a quick battle, and then use
`heal_char` to heal by a specific amount, printing out the result when
the program is exiting. In order to make the simulation a little more
interesting, and to be able to print out the results, let's import
some functions from the C standard library: `srand`, `clock`, `rand`
and `printf`.

We'll begin by importing the functions we'll need from the C `time.h`,
`stdio.h` and `stdlib.h` headers.

```haskell
[ivory|

import (time.h, clock)   uint64_t clock()
import (stdlib.h, srand) void srand(uint64_t x)
import (stdlib.h, rand)  uint16_t rand()
import (stdio.h, printf) void printf_u16(string x, uint16_t y)

|]
```

Make sure to include these in the definition of `exampleModule`, e.g.,
`incl printf_u16`.

Note that when `printf` is imported, it's imported at a specific type
(`Uint16 :-> ()`). This is because Ivory doesn't support
variable-argument functions. If we would like to use `printf` with
different arguments, we need to import it again for each occurrence
(e.g., `printf_u32`, `printf_string`). Luckily, this only pollutes the
Ivory namespace -- the generated code will just contain calls to
`printf`.

Now we define a new procedure to initialize the random number
generator in the C standard library, seeding it with the output of the
`clock` function. We'll define this as a macro again, so that we can
keep our `ivoryMain` function relatively streamlined. We'll also make
another macro that serves as a convenient interface to `rand`, which
will let us constrain its output to a specific range of values.

```haskell
init_rng :: Ivory eff ()
init_rng  =
  do val <- call clock
     call_ srand val

gen_rand :: Uint16 -> Ivory eff Uint16
gen_rand max_val =
  do val <- call rand
     return (val .% max_val)
```

Once the `init_rng` function has been added, modify `ivoryMain` to
include a call to it before the use of `ret 0`. To make sure that
we're generating random values correctly, we'll use the `gen_rand`
macro to produce a value that we can print out. Your `ivoryMain`
procedure should now look like this:

```haskell
ivoryMain :: Def ('[] ':-> Sint64)
ivoryMain  =
  proc "main" $
  body $
    do init_rng
       val <- gen_rand 10
       call_ printf_u16 "val: %d\n" val
       ret 0
```

> NOTE: `call` vs `call_`
>
> In Ivory, there are two ways to call a procedure: `call` and `call_`. The
> difference between the two is how the result is handled. With `call`, the
> result can be named and used in other expressions. With `call_` the result
> cannot be named, and is discarded. For functions that return a value, you
> often want to use `call`, while functions that return nothing, or the `()`
> value should always be called with `call_`.

Now that `ivoryMain` is actually doing more than just returning an
exit code, let's generate, compile, and run the program:

```sh
$ stack codegen.hs --src-dir=out
$ make -C out example
$ ./out/example
val: 2
```

Note that in addition to your random value likely being different, you
may see some compiler warnings when running the `make` step. This is
because C compilers by default expect the return type of `main` to be
a machine-dependent `int`, but all types in Ivory are
machine-independent.

### Simulating a battle

Let's modify the `ivoryMain` function to include a simple battle
simulation. We'll make a single instance of a `Character` struct, do
some damage to it, and then have them cast a healing spell. To do
this, we'll need to implement some more logic: a way to damage the
character, and a way to cast the healing spell.

Let's start with the damage application. We would like to apply a
minimal amount of damage, plus some variable amount of damage that we
can use to to simulate a better or worse attack. Let's re-use the
`gen_rand` macro that was used in testing above to define a new
procedure, `apply_damage`. This procedure will take three arguments:
base damage, a maximum additional damage to apply, and the `Character`
that is the target of the damage.

```haskell
apply_damage :: Def ('[Uint16, Uint16, Ref s ('Struct "Character")] ':-> ())
apply_damage  =
  proc "apply_damage" $ \ base max_additional ref ->
  body $
    do additional <- gen_rand max_additional
       health     <- deref (ref ~> hp)
       damage     <- assign (base + additional)
       store (ref ~> hp) ((damage >? health) ? (0,health - damage))
```

Now we need to be able to cast the heal spell. Let's implement that
as a procedure again, called `heal_spell`. Given a character struct,
this function adds `25%` of that character's maximum health back, but
only if they have at least 10 mp available. We can reuse the
`heal_char` procedure defined above to apply the health, and query the
character's stats to figure out how much health to apply.

```haskell
heal_spell :: Def ('[Ref s ('Struct "Character")] ':-> ())
heal_spell  =
  proc "heal_spell" $ \ ref ->
  body $
    do avail_mp <- deref (ref ~> mp)
       when (avail_mp >? 10) $
         do store (ref ~> mp) (avail_mp - 10)
            maximum_hp <- deref (ref ~> max_hp)
            val        <- assign (maximum_hp `iDiv` 4)
            call_ heal_char val ref
```

At this point, we can modify `ivoryMain` to simulate the
battle. First, we will allocate a new character, and set up their
initial health and magic.  Next, we will damage them using the
`apply_damage` procedure. Finally, we will use the `heal_spell` to
have them recover some health.

```haskell
show_health :: Ref s ('Struct "Character") -> Ivory eff ()
show_health ref =
  do d <- deref (ref ~> hp)
     call_ printf_u16 "Character health: %d\n" d

ivoryMain :: Def ('[] ':-> Sint32)
ivoryMain  =
  proc "main" $
  body $
    do init_rng

       char <- local $ istruct [ hp     .= ival 100
                               , max_hp .= ival 250
                               , mp     .= ival 20
                               , max_mp .= ival 100 ]

       show_health char

       call_ apply_damage 20 20 char
       show_health char

       call_ heal_spell char
       show_health char

       ret 0
```

When running the simulation, we can see the characters health drop by a value
between 20 and 40, then recover by 62 points.

```sh
$ stack codegen.hs --src-dir=out
$ make -C out example
cc     example.c   -o example
$ ./out/example
Character health: 100
Character health: 72
Character health: 134
```

## Concrete syntax

As the simulation stands, there's currently no way for a character to
regain magic points. Additionally, characters can't hold any items,
such as potions. Let's modify the `Character` structure to track the
number of magic-replenishing potions currently held.

```haskell
[ivory|

struct Character
  { uint16_t hp
  ; uint16_t max_hp
  ; uint16_t mp
  ; uint16_t max_mp
  ; uint16_t potions
  }

|]
```

Make sure to update the `ivoryMain` function to include an initializer for the
`potions` label.

Next, let's implement a function to consume one potion and add back 25 points of
magic points to the character. We will use Ivory's concrete syntax, a C-like
syntax for Ivory. In fact, you've already seen pieces of concrete syntax---our
struct definition and import statements are in the concrete syntax. But we have
a concrete syntax for the entire language! So let's write a function
`use_potion` in the concrete syntax.

```haskell
[ivory|

void use_potion(*struct Character c) {
  let ps = *c.potions;

  if (ps > 0) {
    -- decrement the number of available potions
    store c.potions as (ps - 1);

    -- increase mp up to the maximum
    let mp_val = *c.mp;
    let mp_max = *c.max_mp;
    if ((mp_val + 25) < mp_max) {
      store c.mp as (mp_val + 25);
    } else {
      store c.mp as mp_max;
    }
  } else {}
}

|]
```

Make sure to add `use_potion` to the `exampleModule` definition, and then run
`stack codegen.hs --std-out` to see the C definition of the function.

## Bit data

When writing programs for memory-constrained systems, or when
interacting with hardware directly, we often interpret unsigned
integers as arrays of bits, rather than as numbers. Ivory lets us
define these bit data structures with friendly names and specific
types, making it them less error-prone to manipulate.

Let's extend our simulation to add statuses to our character, and
rather than adding each status as a boolean flag on the `Character`
struct, let's use bit data to hold all of those statuses together in
one compact value.

```haskell
[ivory|
bitdata Statuses :: Bits 8 = statuses_data
  { stat_bleeding :: Bit
  , stat_blocking :: Bit
  , stat_silenced :: Bit
  , _             :: Bits 5
  }
|]
```

The members of this declaration all go together, like a struct, but
rather than a collection of pointers, they are all values within a
single scalar byte. We are not yet using all of the bits available to
us, but we must be explicit about those unused bits with `_`.

Let's add a `Statuses` field to the `Character` struct, and use one of
these statuses by reducing the damage a character takes if that
character is blocking:

```haskell
[ivory|

struct Character
  { uint16_t hp
  ; uint16_t max_hp
  ; uint16_t mp
  ; uint16_t max_mp
  ; uint16_t potions
  ; uint8_t  statuses
  }

|]
```

```haskell
apply_damage :: Def ('[Uint16, Uint16, Ref s ('Struct "Character")] ':-> ())
apply_damage  =
  proc "apply_damage" $ \ base max_additional ref ->
  body $
    do additional <- gen_rand max_additional
       health     <- deref (ref ~> hp)
       ss         <- deref (ref ~> statuses)
       blocking   <- assign (bitToBool (fromRep ss #. stat_blocking))
       damage     <- assign (base + (blocking ? (0,additional)))
       store (ref ~> hp) ((damage >? health) ? (0,health - damage))
```

On the line that defines `blocking`, we first go from an unsigned
8-bit integer to a `Statuses` value (`fromRep ss`), project out the
blocking bit (`#. stat_blocking`), and then convert the bit into an
Ivory boolean (`bitToBool`). When the character is blocking, they then
only take the base damage, and none of the additional random damage.

Now blocking affects the damage taken by a character, but we have no
way yet to start or stop blocking. Let's add procedures to do exactly
that:

```haskell
start_blocking :: Def ('[Ref s ('Struct "Character")] ':-> ())
start_blocking  =
  proc "start_blocking" $ \ ref ->
  body $
    do printf_u16 "Character starting to block\n" 0
       withBitsRef (ref ~> statuses) $
         do setBit stat_blocking

stop_blocking :: Def ('[Ref s ('Struct "Character")] ':-> ())
stop_blocking  =
  proc "stop_blocking" $ \ ref ->
  body $
    do printf_u16 "Character no longer blocking\n" 0
       withBitsRef (ref ~> statuses) $
         do clearBit stat_blocking
```

These procedures use `withBitsRef` to introduce another `do` block
where multiple operations can be performed simultaneously on bit
data. In this case, though, we're just setting or clearing a single
bit.

### Extending the simulation

Let's see all this in action by adding a couple more moves to our
simulation, remembering to `incl start_blocking` and
`incl_stop_blocking`:

```haskell
ivoryMain :: Def ('[] ':-> Sint32)
ivoryMain  =
  proc "main" $
  body $
    do init_rng

       char <- local $ istruct [ hp       .= ival 100
                               , max_hp   .= ival 250
                               , mp       .= ival 20
                               , max_mp   .= ival 100
                               , potions  .= ival 3
                               , statuses .= ival 0
                               ]

       show_health char

       call_ apply_damage 20 20 char
       show_health char

       call_ heal_spell char
       show_health char

       call_ start_blocking char
       call_ apply_damage 20 20 char
       show_health char

       ret 0
```

## Exercises

1. Rename `show_health` to `show_status`, and make it print an
   additional message with the character's number of magic points.

2. In `start_blocking` and `stop_blocking`, we print a message using
   `printf_u16`, but the format string does not use the value we pass
   to it. This isn't a major problem, but can cause some annoying
   warnings on newer compilers. Add a new import, `printf_void`, that
   doesn't take any arguments, and convert `start_blocking` and
   `stop_blocking` to use it.

3. Our `stat_bleeding` and `stat_silenced` statuses don't yet do
   anything, and blocking isn't very well-balanced (why wouldn't you
   always block?). Make it so that:
   - When `apply_damage` deals the maximum possible damage (`base +
     max_additional`), the bleeding status is added to the character.
   - When the character is bleeding, `apply_damage` does extra damage.
   - If `heal_spell` is cast successfully, make it remove the bleeding
     status from the character.
   - Whenver the character takes damage, there is a 10% chance the
     character is silenced.
   - `heal_spell` does not work when the character is silenced.
   - Drinking a potion removes silencing, in addition to its usual
     magic point restoring effect.
   - The character cannot drink potions while blocking.

## Digging Deeper

- There are more examples in the directory
  `ivory/ivory-examples/examples/` demonstrating many aspects of the
  language we have not touched on here. For more details of using the
  concrete syntax, see `ivory/ivory-examples/examples/ConcreteFile.hs`
  and `ivory/ivory-examples/examples/file.ivory`.
- Explore the Ivory standard library for useful functions:
  `ivory/ivory-stdlib/`.
- The core Ivory language is defined in
  `ivory/ivory/src/Ivory/Language.hs`.
- We have not described [Tower][tower], our concurrency/architecture
  DSL. Tower can be layered on Ivory to build concurrent systems.

### Advanced Exercises

1. Our game simulation is rather tedious to extend with new
   moves. Instead of writing each move manually, use a loop to run the
   following step 20 times:

```haskell
       call_ apply_damage 20 20 char
       show_health char
```

   If the character's health points ever reach `0`, break out of the
   loop and end the program with a non-zero exit code.

   *Hints:* you will likely want to look up `for`, `breakOut`, and
   `local` in the documentation and examples.

2. Even the luckiest character will not make it through 20 turns of
   damage without blocking, casting their healing spells, and drinking
   potions. Make it so that the character does all three when
   appropriate.

[tower]: https://github.com/GaloisInc/tower
