# Ivory Tutorial

## Environmental Setup

Goals:
* Install stack
* Checkout the Ivory repository
* Use stack to configure an Ivory build environment
* Setup a skeleton project and get it building

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

## Motivating Example

Open the `ivory-tutorial/example.hs` file in your text-editor of choice. It has
some boilerplate filled out for you already, which we'll go through next.

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

With no options given on the command line, the Ivory program will be compiled
with no optimizations or generated checks, and the following files will be
outputted: `ivory.h`, `ivory_asserts.h`, `ivory_templates.h`, `example.h`, and
`example.c`. For ease of testing, you can use the --std-out option to just print
out the relevant parts of the example module we've defined.


### Building the C module

After generating code for an Ivory module, we will need to use a C compiler to
build the artifacts into a program. As our generated code is small, we can just
invoke the compiler directly, rather than building any additional infrastructure
around it:

```sh
$ gcc -o example example.c
$ ./example
```
