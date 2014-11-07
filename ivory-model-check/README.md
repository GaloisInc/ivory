# Ivory Model-Checker

This package provides a means of verifying Ivory programs using the [CVC4][cvc4]
SMT-solver. We symbolically execute the Ivory program to produce a set of
verification conditions, which we then check for validity using CVC4. Thus we
can verify that Ivory procedures satisfy each others (and their own) contracts
(`requires` and `ensures`), as well as user- or compiler-inserted assertions to
guard against things like integer overflow.

## Usage

We require a [development version][dev] of CVC4 at the moment, anything since
2014-10-15 should work.

The main entry-point for this package is `Ivory.ModelCheck.modelCheck`:

```haskell
modelCheck :: Args     -- ^ Options, e.g. verbosity, whether to inline function-calls, etc.
           -> [Module] -- ^ Modules we intend to call into
           -> Module   -- ^ The module we wish to verify
           -> Result
```

The most important option is likely `inlineCall`, which determines how to handle
inter-procedural analysis. The default is `False`, which means we will check the
`requires` clauses and assume the `ensures` clauses; if set to `True`, we will
symbolically execute the callee, which is more expensive but often more precise.

We also strongly recommend using this package with GHC 7.8.3, so you can enable
our compiler plugin that adds source locations to Ivory's AST. The model-checker
will use the source locations to provide more precise error messages.

# Limitations

The model-checker supports most Ivory statements and expressions, with the
following caveats:

- The model-checker will not accept any procedures that use `forever` loops or
  `break` statements. Most Ivory loops are statically bounded, so we just unroll
  them for the maximum number of iterations; but this tactic does not work for
  infinite loops or loops that can return early.
- Floating-point arithmetic modeled *extremely* conservatively, i.e. we assume
  nothing about floating-point operations. If your module makes assertions about
  floating-point values, the model-checker will be unable to prove them.
- Non-linear integer arithmetic is supported, but imprecise. We abstract the
  `*`, `/`, and `%` operators by assuming a few simple identities about them.
- The `inlineCall` flag is designed to be used with non-recursive procedures
  only at the moment. The model-checker will probably enter an infinite loop if
  it tries to inline a recursive procedure. This restriction can be removed if
  there's interest.


[cvc4]: http://cvc4.cs.nyu.edu/web/
[dev]: http://cvc4.cs.nyu.edu/downloads/
