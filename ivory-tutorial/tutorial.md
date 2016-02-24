# Ivory Tutorial

## Environmental Setup

Goals:
* Install stack
* Checkout the Ivory repository
* Use stack to configure an Ivory build environment
* Build documentation (optional)
* Install CVC4 (optional)

### Installing stack

Follow the directions for your OS at
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

Let's make an inventory manager for a simple role-playing game. To start with,
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
printf_u16  = importProc "printf" "stdlib.h"
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

gen_rand :: Uint64 -> Ivory eff Uint64
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
       printf_u16 "val: %d\n" val
       ret 0

```

## Building the example

Now that we have our first Ivory program written, let's generate some code! From
the `ivory-tutorial` repository, run the following command:

```sh
$ stack codegen.hs -h
```

The -h flag will cause the code generator to print out all options that you can
use to configure its behavior. You should see the following list displayed:

```
Usage: example.hs [OPTIONS]

      --std-out           print to stdout only
      --src-dir=PATH      output directory for source files
      --const-fold        enable the constant folding pass
      --overflow          enable overflow checking annotations
      --div-zero          generate assertions checking for division by zero.
      --ix-check          generate assertions checking for back indexes (e.g., negative)
      --fp-check          generate assertions checking for NaN and Infinitiy.
      --bitshift-check    generate assertions checking for bit-shift overflow.
      --out-proc-syms     dump out the modules' function symbols
      --cfg               Output control-flow graph and max stack usage.
      --cfg-dot-dir=PATH  output directory for CFG Graphviz file
      --cfg-proc=NAME     entry function(s) for CFG computation.
      --verbose           verbose debugging output
      --srclocs           output source locations from the Ivory code
      --tc-warnings       show type-check warnings
      --tc-errors         Abort on type-check errors (default)
      --no-tc-errors      Treat type-check errors as warnings
      --sanity-check      Enable sanity-check
      --no-sanity-check   Disable sanity-check
  -h  --help              display this message
```

With no options given on the command line, the C generated will have had no
optimizations applied, or checks generated. The following files will be
outputted: `ivory.h`, `ivory_asserts.h`, `ivory_templates.h`, `example.h`, and
`example.c`. For ease of testing, you can use the --std-out option to print out
the relevant parts of the example module, instead of writing out files.


### Building the C module

After generating code for an Ivory module, we will need to use a C compiler to
build the artifacts into a program. As our generated code is small, we can just
invoke the compiler directly, rather than building any additional infrastructure
around it:

```sh
$ gcc -o example example.c
$ ./example
```

## Tools

### Symbolic Simulator

Ivory comes with a symbolic simulator that can be used to prove the validity of
assertions in a function. To aid the symbolic simulator, the Ivory language also
provides a way to define function contracts, aiding compositional verification.

Let's look at what simple additions we can make to the running example to aid in
verification of behavior.
