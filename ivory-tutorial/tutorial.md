# Ivory Tutorial

## Environmental Setup

Goals:
* Install stack
* Checkout the Ivory repository
* Use stack to configure an Ivory build environment
* Build documentation (optional)
* Install CVC4 (optional)

### Installing stack

Stack is a tool for managing a repository of Haskell source. We're going to use
it to build all of the libraries in the Ivory repository, and then our own
example application. Stack can also build and maintain a documentation index,
which can ease development.

To install stack, follow the directions for your OS at
[haskellstack.org](http://docs.haskellstack.org/en/stable/README/#how-to-install)

### Checkout the Ivory repository

If you have git installed, you can just issue the following command:
```sh
$ git clone https://github.com/galoisinc/ivory
```

If not, just click the `download zip` button on the [ivory github
page](https://github.com/galoisinc/ivory), and unzip the archive.

### Configure the build environment

Using a terminal, change to the checked out or unzipped Ivory repository. Now,
simply run `stack build` to setup the build environment.

```sh
$ stack build
```

### Build documentation (optional)

In order to navigate the documentation a little easier, we're going to build the
documentation for all of the packages in the Ivory ecosystem. To generate docs,
simply type:

```sh
$ stack haddock
```

Once this command finishes (and it will take a while) it will print out the
location of three documentation indexes: one for local packages, one for local
packages and dependencies, and one for just dependencies. The index for local
packages will contain documentation for the Ivory packages only, and will be the
most helpful when writing Ivory programs.

> NOTE: the location printed will look something like this, with `<ivory>`, and
> `<arch>` being something specific to your local install:
>
> `<ivory>/.stack-work/install/<arch>/lts-5.3/7.10.3/doc/index.html`

### Install CVC4 (optional)

To make use of the symbolic simulator for Ivory, you will need to install CVC4.
If you don't plan on using this tool, you can skip this section.

Please follow the installation directions located at
[the cvc4 download page](http://cvc4.cs.nyu.edu/downloads/).


## The example skeleton

Open the `ivory-tutorial/Example.hs` file in your text-editor of choice. It has
some boilerplate filled out for you already, which we'll go through now.

### The `exampleModule` definition

Ivory calls a compilation unit a module. Modules are defined by using the
`package` function, which places a collection of named declarations within a
single module. You can see this in effect at line 14, where the `ivoryMain`
function is included in the module by using the `incl` statement.

### The `ivoryMain` procedure

Programs in Ivory are composed of a collection of procedures. The `ivoryMain`
procedure is introduced in lines 16-24, as a procedure that simply returns `0`.
It also has a type signature, that reflects this fact -- `ivoryMain` accepts no
formal parameters (the `'[]` to the left of the `':->` symbol) and returns a
value of type `Uint32`. The structure of the `ivoryMain` procedure is simple --
it only includes a single statement that causes it to return immediately (the
use of the `ret` statement). As the tutorial progresses, we'll modify the body a
bit, and have it do something a bit more interesting.

### A note on Haskell syntax

You may have noticed that in the type signature for `ivoryMain` that some
portions of the signature have a leading single-quote. This is due to a language
extension in GHC called `DataKinds`, which allows data constructors to be
promoted to the type level. For the most part you won't need to know about this
feature, other than it needing to be enabled in an Ivory module, and GHC will
even tell you when you've forgotten to use the leading single-quote, as long as
you're compiling with `-Wall`.

Also present in many Haskell (and Ivory) programs is the `$` operator. While it
may look strange and somewhat out of place, the `$` operator is actually quite
benign: it is just a name for function application. The reason that `$` gets so
much use is that in many cases, it allows for pairs of parenthesis to be
removed. For example, in the following example, values `x` and `y` are the same.

```haskell
x = f (g (h 10))
y = f $ g $ h 10
```

### The `main` function in `ivory-tutorial/codegen.hs`

The main function in `codegen.hs` just serves as an entry-point to the code
generator. As we're only building one Ivory module, we give a list of a single
element as the first argument, and as there are no additional artifacts that we
would like bundled, we give an empty list as the second. You can learn more
about other ways to invoke the C code generator in the documentation for the
`ivory-backend-c` package.

### The `main` function in `ivory-tutorial/simulate.hs`

The main function in `simulate.hs` invokes the symbolic simulator on the
`ivoryMain` function from the `ivoryExample` module, defined in `Example.hs`. If
you run it with `stack simulate.hs`, you'll notice that it prints out an
encoding of the Ivory program from `Example.hs` to CVC4's input language and
finally, `Safe`. This procedure is trivially safe, as all it does is return the
value 0, making no assertions along the way.

## Motivating example

Let's make a character manager for a simple role-playing game. To start with,
we'll need some basic stats about our character. Let's start defining a `struct`
that we can use to keep track of some basic character information. Add this
chunk of code to `Example.hs`:

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

This will be enough information to keep track of how much health and mana our
character has available. To make sure that it will be included in the generated
module, add the following line after the `incl ivoryMain` line in the definition
of `exampleModule`:

```haskell
       defStruct (Proxy :: Proxy "Character")
```

Just to confirm that it works, run `stack codegen.hs --std-out`, and see that
the `struct` declaration has made it into the output.  Next, we'll need
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

recover_mp :: Def ('[Uint16, Ref s ('Struct "Character",] ':-> ())
recover_mp  =
  proc "recover_mp" $ \ amount ref ->
  body $
    do current_mp <- deref (ref ~> mp)
       total_mp   <- deref (ref ~> max_mp)
       new        <- assign (current_mp + amount)
       store (ref ~> mp) ((new >? total_mp) ? (total_mp,new))
```

Make sure to add `incl heal_char` and `incl recover_mp` to the definition of
`exampleModule`. Now, running `stack codegen.hs --std-out` should show the two
procedures. Examining the output, and the original source program, you'll
probably notice that the two look quite similar. It would be nice to abstract
the core of the operation, which would reduce the possibility for bugs to creep
in.

In Ivory, we have two options for doing this: we could just make another
procedure and call it, or we could write a macro for the behavior, and inline it
into the call site. Let's have a look at each approach:

```haskell
add_var :: Def ('[Uint16, Ref s ('Stored Uint16), Ref s' ('Stored Uint16)] ':-> ())
add_var  =
  proc "add_var" $ \ amount var max_var ->
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

Make sure to add `incl add_var` to the definition of `exampleModule` so that the
new function will be included in the generated module.  In this version, you
should see that the implementations of `heal_char` and `recover_mp` simply
turned into calls to `add_var`, passing the relevant references to `hp` or `mp`
in to each call. Now let's contrast that with the macro version:


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

In this version, `add_var` has been turned into a normal Haskell function that
produces results in the `Ivory` monad. Note that we don't need to export the
`add_var` procedure in the `exampleModule` anymore, and that we no longer need
to use the `call_` function when using `add_var`. When you generate code, you
should see that the implementation of `heal_char` and `recover_mp` look exactly
the same as they did originally, but we've factored out the implementation into
one place so that we don't expose ourselves to the same copy-paste bug that
existed before.

Let's continue on using the macro version, and write up a simple scenario in the
`ivoryMain` procedure. To test the `heal_char` functionality, let's simulate a
quick battle, and then using `heal_char` to heal by a specific amount, printing
out the result when the program is exiting. In order to make the simulation a
little more interesting, and to be able to print out the results, let's import
some functions from the C standard library: `srand`, `clock`, `rand` and `printf`.

Let's begin by importing the functions we'll need from the C `time.h`, `stdio.h`
and `stdlib.h` headers. Make sure to include these definitions in the definition
of `exampleModule`. Note that when `printf` is imported, it's imported at a
specific type (`Uint16 :-> ()`). The purpose for this is that Ivory doesn't
support variable-argument functions. If we would like to use `printf` with
different arguments, we need to import a new symbol for each occurrence.
Luckily, this only pollutes the Ivory namespace -- the generated code will only
ever call `printf`.

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

Now, we can define a new procedure to initialize the random number generator in
the C standard library, with the output of the `clock` function. Let's define
this as a macro again, so that we can keep our `ivoryMain` function relatively
streamlined. At the same time, let's make another macro that serves as an
interface to the C function `rand`, which will streamline its use, constraining
its output to a specific range of values.

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

Once the `init_rng` function has been added, modify `ivoryMain` to include a
call to it before the use of `ret 0`. To test this out, and make sure that we're
generating random values correctly, let's use the `gen_rand` macro to produce a
value that we can print out. Your `ivoryMain` procedure should now look like
this:

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
will output a few C headers, and a single C source file to the current
directory.

```sh
$ stack codegen.hs
$ ls -F
codegen.hs         example.c          example.hs         ivory_asserts.h    simulate.hs
example*           example.h          ivory.h            ivory_templates.h  tutorial.md
```

Now, we can build the example module by using gcc (or your preferred C
compiler).

```sh
$ gcc -o example example.c
$ ./example
val: 3
$ ./example
val: 8
```

### Simulating a battle

Now, let's modify the `ivoryMain` function to include a simple battle
simulation. We'll make a single instance of a `Character` struct, do some damage
to it, and then simulate them invoking a healing spell. To do this, we'll need
to implement some more logic: a way to damage the character, and the healing
spell.

Let's start with the damage application. We would like to apply a minimal amount
of damage, plus some variable amount of damage that we can use to to simulate a
better/worse attack. Let's re-use the `gen_rand` macro that was used in testing
above to aid in the definition of a new procedure: `apply_damage`. This
procedure will take three arguments: base damage, a maximum additional damage
to apply, and the `Character` that is the target of the damage.

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

Now we need to be able to invoke the heal spell. Let's implement that as a
procedure again, called `heal_spell`. What this function will do is given a
character, add `25%` of that character's maximum health back, but only if they
have at least 10 mp available. We can reuse the `heal_char` procedure defined
above to apply the health, and query the character's stats to figure out how
much health to apply.

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
            call_ heal_char val ref (
```

At this point, we can modify `ivoryMain` to simulate the battle. First, we will
allocate a new character, and setup their initial health and magic quantities.
Next, we will damage them using the `apply_damage` procedure. Finally, we will
use the `heal_spell` to have them recover some health.

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
$ ./example
Character health: 100
Character health: 72
Character health: 134
```

## Symbolic Simulator

Ivory comes with a symbolic simulator that can be used to prove the validity of
assertions in a function. To aid the symbolic simulator, the Ivory language also
provides a way to define function contracts, aiding compositional verification.

Let's look at what simple additions we can make to the running example to aid in
verification of behavior.

### Adding contracts

Many of the functions that we have defined have built-in assumptions about their
arguments, but there is currently no formal specification that communicates
them. For example, the `heal_char` function assumes that the character's health
is not already above the `max_hp` value stored in the `Character` struct.
Additionally, many procedures defined above provide undocumented guarantees
about the values they return, and the effects they provide -- the `heal_char`
procedure won't put the character's health above it's `max_hp` value.

We can modify the `heal_char` procedure to more fully specify the contract that
it obeys, with the addition of the `requires` and `ensures` clause to its
definition:

```haskell
valid_health ref =
  checkStored (ref ~> hp)     $ \ hp_val ->
  checkStored (ref ~> max_hp) $ \ max_hp_val ->
    hp_val <=? max_hp_val .&& (maxBound - max_hp_val) >=? amt

heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  requires (valid_health ref) $
  ensures_ (valid_health ref) $
  body $
    add_var amount (ref ~> hp) (ref ~> max_hp)
```

The new predicate that we defined, `valid_health`, states that the `hp` value of
the `Character` struct given will never be above the `max_hp` value.
Additionally it specifies that the amount that will be added to the `hp` value
shouldn't cause an overflow when the check is performed in the `add_var` macro.
The `valid_health` predicate is applied both as a precondition, and
postcondition, as the `heal_char` procedure both requires it to be true, and
preserves the invariant specified.

To verify that this procedure respects its contract, modify the `main` function
in `simulate.hs`, to check `heal_char` instead of `ivoryMain`, and run
`stack simulate.hs`. What you should see is the embedding to CVC4 printed, and
then the last line of output should be `Safe`. This indicates that there were no
overflow problems, and that the procedure was shown to respect its contract.

As an experiment, try removing the `.&& (maxBound - max_hp_val) >=? amt` portion
of the predicate in `valid_health`, and re-running `stack simulate.hs`. What you
should see is that instead of printing `Safe` at the end, it prints `Unsafe` and
some context about what caused the problem (`QUERY ovf0`). If you look back
through the encoding, `ovf0` will be an assertion about the `hp` value not
overflowing when the healing amount is added to it. Without specifying that
adding the amount to `hp` shouldn't overflow, the simulator is unable to prove
that it won't. Note that adding this as a precondition isn't the end of the
problem, as now all call-sites for `heal_char` will now need to prove that the
character's health stats are in a state where healing them won't cause overflow.
