# intlang

A small functional language called intlang, that I came up with (wow super original, I know) to get some experience with writing a Parser. This is about trying and probably getting a bunch of things wrong but learn from the mistakes.

## Syntax by Example
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
in ´/test/cases´ there are plenty more examples (.expect files are the expected results).

## Application and Lambda
- `a b` applies a to b, application binds strongest.
- `\a. b` creates a local binding for the Var a on the Expression b. Lambdas bind weakest.

## Binary Operators
- `*`,`-`,`+`,`<`,`==` (left to right => decreasing binding strength)
- Hint: Use `1-...` to do negation, `*` for AND and `+` for OR
- Given `a*b` if a evaluates to 0, b will not be evaluated (allows for recursion base/recursion case distinction)

### What about if else
- Say "if a>5 then 99 else 111" can be written as: `(a>5)*99 + (1-(a>5))*111`
- I never said it is a nice language :|

## let
- There are global definitions of the form: `let <id> = <lambda expression> ;`
- They are SSA but local lambda variables my shadow the global ones (and other lambda vars)

## Program Structure
- There may be some `let` statements in the beginning of the program
- It must end with an Expression (see examples)

## Types
- There is only one Atomic Type, the int 

## Vars
- Variable names must start with a Letter (not a Number)

## How to Run

- To build `dune build`
- To run some program (here fib.intlang): `dune exec bin/main.exe -- fib.intlang`
- To run the tests: `dune test`

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
