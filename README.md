# intlang v4

A small eager functional language with 
- Immutable n dimensional vectors accelerated by a functional but in place optimization
- No Garbage Collector just compile time known allocation and freeing manged by the compiler
- N ary Tuples
- i32 and i8 basic data types
- A stdlib with basics like IO or typical functional programming helpers like fold

A small functional language called intlang, that I came up with (wow super original, I know) to get some experience with writing a Parser. This is about trying and probably getting a bunch of things wrong but learn from the mistakes. The project a evolved a bit past what I originally imagined. 

As of now it has Lexer, Parser, Include Resolver, Typechecker, Monomorphization, Interpreter and some Tests :)

## Language Descriotion

`intlang.md` contains an informal description on how to code in intlang. The tests (in `test/cases/`) and std lib (in `test/intlangstdlib/`) are also a good source of inspiration.

## How to Run

- To build `dune build`
- To run some program (here fib.intlang): `dune exec bin/main.exe -- fib.intlang`
- To run the tests: `dune test`

## Dependencies (not complete)

- LLVM 19 (also respective ocaml package) with clang for compilation

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
- https://github.com/mgrabmueller/AlgorithmW
