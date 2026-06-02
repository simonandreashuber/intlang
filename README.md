# intlang v3

A small functional language called intlang, that I came up with (wow super original, I know) to get some experience with writing a Parser. This is about trying and probably getting a bunch of things wrong but learn from the mistakes. The project a evolved a bit past what I originally imagined. 

As of now it has Lexer, Parser, Typechecker Interpreter and some Tests :)

## Formal Semantics and Type System

in ´doc/formal-semantics-and-type-system.tex´ there are notes on:
 - Lexical Conventions
 - Concrete Grammar (BNF)
 - Type System
 - Evaluation Strategy

## Syntax by Example

in ´/test/cases´ and ´/test/intlangstdlib´ there are plenty of examples. Here are also some

```
let sqr = \x. x * x;
let inc = \x. x + 1;
sqr (inc 5) == 36
>>> 1
```

```
let inrange = \a.\x.\b. (a<x)*(x<b)
inrange 1 2 3
>>> 1
```

```
include mod
let answers = \x. if mod x 2 then vec[6,7] else vec[4,2] end;
(answer 3).1
>>> 7
```

```
include vector
vector.vec_left_fold (\x. \y. x+y) 0 vec[1,2,3,4,5];
>>> 15
```

```
include mat

-- 2x3 mat
let A = vec[1,2,3,
            4,5,6];
-- 3x4 mat
let B = vec[7,8,9,10,
            11,12,13,14,
            15,16,17,18];

let AB = mat.matmul 2 3 4 A B;
vecget[AB,0]
>>> 74
```

## How to Run

- To build `dune build`
- To run some program (here fib.intlang): `dune exec bin/main.exe -- fib.intlang`
- To run the tests: `dune test`

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
- https://github.com/mgrabmueller/AlgorithmW
