*************************************************************************************************
* An interpreter for a slight extension of the untyped lambda calculus with Scheme-like syntax. *
*************************************************************************************************

To run the program, install Haskell, Cabal, and the necessary dependencies, and then type:
  cabal run HaskellLambda

// TODO: give specific steps to set up environment

The executable can be found at dist-newstyle/build/x86_64-linux/ghc-9.4.8/HaskellLambda-0.1.0.0/x/HaskellLambda/build/HaskellLambda/HaskellLambda.


**********
* Syntax *
**********

The core syntax of the language is as follows:
<term> ::=
    | <var>
    | (lambda (<var>*) <term>)
    | (<term> <term>*)
    | #true
    | #false
    | (if <term> <term> <term>)
    | <int>

There are a few primitive functions, namely +, -, *, /, <, <=, =, >, >=, and /=.


****************
* Architecture *
****************
- app/Main.hs contains the REPL code.
- lib/Term.hs contains the type declarations for Dbg and Term as well and their core related functions.
- lib/Parser.hs contains the tokenizer and parser code.
- lib/Interpreter.hs contains the interpreter code, defined in terms of eval and apply.  It also contains a helper function to interpret a string directly, rather than a Term.
- tests/Test.hs contains the test cases, written using the HUnit framework.
