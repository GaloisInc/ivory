# Ivory Tutorial

## Environmental Setup

Goals:

- Install stack
- Checkout the Ivory repository
- Use stack to configure an Ivory build environment
- Build documentation (optional)
- Install CVC4 (optional)

### Installing stack

Stack is a tool for managing a repository of Haskell source. We're going to use
it to build all of the libraries in the Ivory repository, and then our own
example application. Stack can also build and maintain a documentation index,
which can ease development.

To install stack, follow the directions for your OS at
the [stack README][stack-install].

[stack-install]: http://docs.haskellstack.org/en/stable/README/#how-to-install

### Checkout the Ivory repository

If you have git installed, you can check out the repository directly:

```sh
$ git clone https://github.com/galoisinc/ivory
$ git checkout wip/tutorial
```

If you do not have git, click the `download zip` button on the
[Ivory github page][ivory-github], and unzip the archive.

[ivory-github]: https://github.com/galoisinc/ivory/tree/wip/tutorial

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

Once the stack build finishes, and it will take a while, it will print the
location of three documentation indexes: one for local packages, one for local
packages and dependencies, and one for just dependencies. The index for local
packages will contain documentation for the Ivory packages only, and will be the
most helpful when writing Ivory programs.

The location printed will look something like this, with `$ivory_repo`, and
`$arch` being something specific to your system:

```
$ivory_repo/.stack-work/install/$arch/lts-5.3/7.10.3/doc/index.html
```

### Install CVC4 (optional)

To use the symbolic simulator for Ivory, you will need to install
CVC4. If you don't plan on using this tool, you can skip this
section. Follow the installation instructions at
[the CVC4 download page][cvc4-download].

[cvc4-download]: http://cvc4.cs.nyu.edu/downloads


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
modules.  The Haskell compiler, when compiling with `-Wall`, will
remind you when you've forgotten to use the leading single-quote.

The `$` operator is a common sight in many Haskell programs, including
Ivory. While it may look strange `$` is a name for function
application. Because of its associativity, Haskell programs often use
`$` in place of pairs of parentheses. For example, without `$`,
`ivoryMain` would look like:

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

### The `main` function in `ivory-tutorial/simulate.hs`

The main function in `simulate.hs` invokes the symbolic simulator on
the `ivoryMain` function from the `ivoryExample` module, defined in
`Example.hs`. If you run it with `stack simulate.hs`, it prints out
the Ivory program from `Example.hs` in CVC4's input language followed
by `Safe`. This procedure is trivially safe, as all it does is return
the value 0, making no assertions along the way.

## Game character example

Let's make a character manager for a simple role-playing game. To
begin, we'll need some basic stats about our character, so we'll
define a `struct` to store the stats. Add this definition to
`Example.hs`:

```haskell
[ivory|

struct Character
  { hp     :: Stored Uint16
  ; max_hp :: Stored Uint16
  ; mp     :: Stored Uint16
  ; max_mp :: Stored Uint16
  }

|]

```

This will be enough information to keep track of how much health
points and magic points our character has. To make sure that it will
be included in the generated module, add the following line after the
`incl ivoryMain` line in the definition of `exampleModule`:

```haskell
       defStruct (Proxy :: Proxy "Character")
```

Haskell is indentation sensitive, so make sure that the `d` lines up just under the `i` of `incl` on the previous line.

Just to confirm that it works, run `stack codegen.hs --std-out`, and see that
the `struct` declaration has made it into the output.  That command tells Stack to run the Ivory compiler, specialized for this program.  (You might also try `stack codegen.hs --help` to see all of the compiler options.)

Next, we'll need
functions to interact with `Character` values.  Let's start by adding a few
utility functions for manipulating health and magic.

```haskell
heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
    do current_hp <- deref (ref ~> hp)
       total_hp   <- deref (ref ~> max_hp)
       new        <- assign (current_hp + amount)
       store (ref ~> hp) ((new >? total_hp) ? (total_hp,new))

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

In Ivory, we have two options for doing this: we could just make
another procedure to be generated in C, and call it from the C code in
both functions. Since we have Haskell as the host language of Ivory,
we can also use Haskell as a macro language for Ivory, defining the
behavior once and inlining it into the definition of both
functions. Let's have a look at the first approach, which is more
similar to how this abstraction would look in C:

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
`hp` or `mp`. Let's contrast that with the macro version:

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

Be sure to remove the `incl add_var` from the `exampleModule`
definition. In this version, `add_var` has been turned into a normal
Haskell function that produces results in the `Ivory` monad, rather
than directly producing an Ivory `Def`. Note that we no longer need to
use the `call_` function when using `add_var`. When you generate code,
you should see that the implementation of `heal_char` and `recover_mp`
look exactly the same as they did in our original version, but we've
factored out the implementation into one place so that we don't expose
ourselves to the same sorts of copy-paste bugs that existed before.

Let's continue using the macro version, and write a simple
game scenario in the `ivoryMain` procedure. To test the `heal_char`
functionality, we'll simulate a quick battle, and then use
`heal_char` to heal by a specific amount, printing out the result when
the program is exiting. In order to make the simulation a little more
interesting, and to be able to print out the results, let's import
some functions from the C standard library: `srand`, `clock`, `rand`
and `printf`.

We'll begin by importing the functions we'll need from the C `time.h`,
`stdio.h` and `stdlib.h` headers. Make sure to include these in the
definition of `exampleModule`.

```haskell
clock :: Def ('[] ':-> Uint64)
clock  = importProc "clock" "time.h"

srand :: Def ('[Uint64] ':-> ())
srand  = importProc "srand" "stdlib.h"

rand :: Def ('[] :-> Uint16)
rand  = importProc "rand" "stdlib.h"

printf_u16 :: Def ('[IString,Uint16] :-> ())
printf_u16  = importProc "printf" "stdio.h"
```

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

### Building the C module

At this point, let's take a break from writing code, and compile the generated
module. This will let us play around with the design, and make sure that
everything is working. Run the `codegen.hs` module with no arguments, and it
will output a few C headers and a single C source file to the current
directory.

```sh
$ stack codegen.hs
$ ls -F
codegen.hs         example.c          Example.hs         ivory_asserts.h    simulate.hs
example*           example.h          ivory.h            ivory_templates.h  tutorial.md
```

Now, we can build the example module by using gcc (or your preferred C
compiler). Make has a built-in rule that can do everything for us and saves a
bit of typing:

```sh
$ make example
cc     example.c   -o example
$ ./example
val: 3
$ ./example
val: 8
```

### Simulating a battle

Let's modify the `ivoryMain` function to include a simple battle
simulation. We'll make a single instance of a `Character` struct, do
some damage to it, and then have them cast a healing spell. To do
this, we'll need to implement some more logic: a way to damage the
character, and the healing spell.

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
       let damage = base + additional
       store (ref ~> hp) ((damage >? health) ? (0,health - damage))
```

Now we need to be able to invoke the heal spell. Let's implement that
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
            let val = maximum_hp `iDiv` 4
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
$ stack codegen.hs
$ make example
cc     example.c   -o example
$ ./example
Character health: 100
Character health: 72
Character health: 134
```

## Symbolic Simulator

Ivory has a symbolic simulator that can prove the validity of
assertions in a function. To aid the symbolic simulator, the Ivory
language can express contracts on individual functions, making it
easier to do verification in small, composable pieces.

In this section, we will see how we can add verification to our game
example.

### Adding contracts

For many of the functions in our example, we have assumptions in our
minds about what properties their arguments must satisfy, but there is
currently no formal specification that expresses those
assumptions. For example, the `heal_char` function assumes that the
character's health is not already above the `max_hp` value stored in
the `Character` struct.  Additionally, we have many assumptions about
the values these procedures return, and the effects they perform. For
example, the `heal_char` procedure won't put the character's health
above it's `max_hp` value.

We can augment `heal_char` procedure with the contract that we expect
it to obey by adding `requires` and `ensures` clauses to its
definition:

```haskell
valid_health ref amount =
  checkStored (ref ~> hp)     $ \ hp_val ->
  checkStored (ref ~> max_hp) $ \ max_hp_val ->
    hp_val <=? max_hp_val .&& (maxBound - max_hp_val) >=? amount

heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  requires (valid_health ref amount) $
  ensures_ (valid_health ref amount) $
  body $
    add_var amount (ref ~> hp) (ref ~> max_hp)
```

The new predicate that we defined, `valid_health`, states that the
`hp` value of the `Character` struct given will never be above the
`max_hp` value.  Additionally it specifies that the amount that will
be added to the `hp` value shouldn't cause an overflow when the check
is performed in the `add_var` macro.  The `valid_health` predicate is
asserted both as a precondition and a postcondition, since the
`heal_char` procedure both requires it to be true initially, and
preserves it as an invariant.

To verify that this procedure respects its contract, modify the `main`
function in `simulate.hs` to check `heal_char` instead of `ivoryMain`,
and then run `stack simulate.hs`. You should see the CVC4 embedding of
the program, and the last line of output should be `Safe`. This
indicates that there were no overflow problems, and that the procedure
was shown to respect its contract.

As an experiment, try removing the `.&& (maxBound - max_hp_val) >=?
amt` portion of the predicate in `valid_health`, and re-running `stack
simulate.hs`. Instead of seeing `Safe` at the end, you should see
`Unsafe` and some context about what caused the problem, `QUERY
ovf0`. If you look back through the encoding, `ovf0` is an assertion
about the `hp` value not overflowing when the healing amount is added
to it. Without specifying that adding the amount to `hp` shouldn't
overflow, the simulator is unable to prove that it won't. Note that
adding this as a precondition isn't the end of the problem, as now all
call sites for `heal_char` must prove that the arguments to the
procedure won't cause overflow.

## Concrete syntax

As the simulation stands, there's currently no way for a character to
regain magic points. Additionally, characters can't hold any items,
such as potions. Let's modify the `Character` structure to track the
number of magic-replenishing potions currently held.

```haskell
[ivory|

struct Character
  { hp     :: Stored Uint16
  ; max_hp :: Stored Uint16
  ; mp     :: Stored Uint16
  ; max_mp :: Stored Uint16
  ; potions:: Stored Uint8
  }

|]
```

Make sure to update the `ivoryMain` function to include an initializer for the
`potions` label.

Next, let's implement a function to consume one potion and add back 25
points of magic points to the character. We will use Ivory's concrete
syntax, defining it within the same block as the `Character` struct
definition.

```haskell
[ivory|

struct Character
  { hp     :: Stored Uint16
  ; max_hp :: Stored Uint16
  ; mp     :: Stored Uint16
  ; max_mp :: Stored Uint16
  ; potions:: Stored Uint8
  }

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

## Exercises

  1. Add a new predicate `potions_available` that checks that the character
     has at least one potion.

  1. Using the new predicate, write a `check_potion_use` function to check that
     `use_potion` requires the character to have at least one potion. Note: You
     will need to update simulate.hs to check `check_potion_use`.

  1. *Extra Credit:* Write a predicate to ensure that after using a potion the
     number of potions is strictly less than before using a potion and make the
     necessary changes to `check_potion_use` to add an `ensures_` statement to
     `check_potion_use`.

## Diving deeper
- There are more examples in the directory `ivory/ivory-examples/examples/` demonstrating many aspects of the language we have not touched on here. For more details of using the concrete syntax, see `ivory/ivory-examples/examples/ConcreteFile.hs` and `ivory/ivory-examples/examples/file.ivory`.
- Explore the Ivory standard library for useful functions: `ivory/ivory-stdlib/`.
- The core Ivory language is defined in `ivory/ivory/src/Ivory/Language.hs`.
- We have not described [Tower][tower], our concurrency/architecture DSL. Tower can be layered on Ivory to build concurrent systems.

[tower]: https://github.com/GaloisInc/tower
