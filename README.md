# HaskellLambda
An interpreter for a slight extension of the untyped lambda calculus with Scheme-like syntax using eager locally nameless terms (i.e. bound variables use de Bruijn indices, free variables use names).  Input expressions can be interpreted directly or lowered to administrative normal form intermediate representation for interpretation.


## Setup
Follow the instructions at [the Haskell Getting Started page](https://www.haskell.org/get-started/) to set up a Haskell environment, including GHCup, HLS, and Stack.  Install the following depencies:
```
cabal install unordered-containers --lib
cabal install text --lib
cabal install tuple-sop --lib
cabal install regex-compat --lib
cabal install extra --lib
cabal install mtl --lib
cabal install utility-ht --lib
cabal install transformers --lib
cabal install transformers-either --lib
cabal install HUnit --lib
```

To run the program, type:
```
cabal run HaskellLambda
```

To run the tests, type:
```
cabal test
```

The executable can be found at dist-newstyle/build/x86_64-linux/ghc-9.4.8/HaskellLambda-0.1.0.0/x/HaskellLambda/build/HaskellLambda/HaskellLambda.


## Syntax

The core syntax of the language is as follows:
```
<term> ::=
    | <var>
    | (lambda (<var>*) <term>)
    | (<term> <term>*)
    | (let ((<var> <term>)*) <term>)
    | #true
    | #false
    | (if <term> <term> <term>)
    | <int>
    | ()
```

Variables can be any valid UTF-8 string separated by parentheses or whitespace.  There are a few primitive functions, namely +, -, *, /, <, <=, =, >, >=, and /=.  These expect two integer arguments and return either an integer or a boolean.

<!-- TODO: provide examples of valid expressions -->
<!-- TODO: explain the semantics -->

## Architecture

| File | Purpose |
| --- | --- |
| app/Main.hs | REPL (Read Evaluate Print Loop) |
| lib/Term.hs | Type declarations for Dbg and Term and their core functions |
| lib/Parser.hs | Tokenizer and parser.  The parser also handles replacing bound variables with de Bruijn indices |
| lib/Interpreter.hs | Interpreter defined in terms of eval and apply |
| lib/ANF.hs | Type declarations for ANF expressions and functions to lower Terms to ANF and rebind variables |
| lib/InterpretAnf.hs | Interpreter for ANF expressions defined in terms of eval and apply |
| tests/Test.hs | Test cases written using the HUnit framework |
