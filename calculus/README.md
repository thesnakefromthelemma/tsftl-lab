A repository of 2 + 3 + 3 + 2 = 10 Haskell implementations and 1 Rust implementation of the standard discrete-calculus-based algorithm (quadratic time complexity and linear space complexity) to compute the least-degree polynomial interpolation of a function defined on an evenly-spaced set of inputs.

The implementations vary according to the container type used to cache the values of the function being interpolated (lists, purely functional arrays, mutable arrays in the `ST` monad, and linear mutable arrays), the levity of said cached values (boxed, unboxed), and the eagerness of the underlying evaluation strategy.

Accordingly, the bulk of the source code is in the following five files:
- [src/List.hs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/src/List.hs)
- [src/Vector.hs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/src/Vector.hs)
- [src/MVector.hs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/src/MVector.hs)
- [src/MVector.rs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/src/MVector.rs)
- [src/Linear.hs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/src/Linear.hs)

There are also short profiling programs in
- [bench/Main.hs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/bench/Main.hs)
- [bench/Main.rs](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/bench/Main.rs)

All the Haskell profiling (including the generation of heap profile eventlogs using `eventlog2html`) can be run at once using the script [Prof.sh](https://github.com/thesnakefromthelemma/tsftl-lab/blob/master/calculus/Prof.sh).
A major goal of this experiment was to assess the feasibility of `linear-base`'s current linear mutable arrays as alternatives to the more familiar wrappers atop GHC's `MutableArray#` and `MutableByteArray#`; the results were disappointing.

I also attempted to verify the correctness of the array-based algorithms (which make liberal use of `unsafe*` functions to allocate/read/write without overhead from bounds checks), but did not get very far.
The partial results can be found in [liquid](https://github.com/thesnakefromthelemma/tsftl-lab/tree/master/calculus/src/Liquid) and [src/Liquid](https://github.com/thesnakefromthelemma/tsftl-lab/tree/master/calculus/liquid); note that the project does not compile with the liquidhaskell plugin unless the flag `-f +liquid` is passed to `cabal`.

The directory [math](https://github.com/thesnakefromthelemma/tsftl-lab/tree/master/calculus/math) is meant to eventually contain a short TeXed overview of the theory (i.e., the discrete calculus) behind the algorithm being implemented, but this is not currently a high priority...