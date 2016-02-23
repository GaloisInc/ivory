# Ivory Tutorial

## Environmental Setup

Goals:
* Install stack
* Checkout the Ivory repository
* Use stack to configure an Ivory build environment
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
> `<ivory>/.stack-work/install/<arch>/lts-5.3/7.10.3/doc/index.html` file.

### Install CVC4 (optional)

To make use of the symbolic simulator for Ivory, you will need to install CVC4.
If you don't plan on using this tool, you can skip this section.

Please follow the installation directions located at
[the cvc4 download page](http://cvc4.cs.nyu.edu/downloads/).


## Motivating Example

Open the `ivory-tutorial/example.hs` file in your text-editor of choice. It has
some boilerplate filled out for you already, which we'll go through now.

### The `main` function

The main function in `example.hs` just serves as an entry-point to the code
generator. As we're only building one Ivory module, we give a list of a single
element as the first argument, and as there are no additional artifacts that we
would like bundled, we give an empty list as the second. You can learn more
about other ways to invoke the C code generator in the
[documentation](https://hackage.haskell.org/package/ivory-backend-c).

## Building the example

Now that we have our first Ivory program written, let's generate some code! From
the `ivory-tutorial` repository, run the following command:

```sh
$ stack example.hs -h
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
