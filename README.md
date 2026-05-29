# intlang v3

A small functional language called intlang, that I came up with (wow super original, I know) to get some experience with writing a Parser. This is about trying and probably getting a bunch of things wrong but learn from the mistakes. The project a evolved a bit past what I originally imagined. I decided to add new futures to the language, namely: If else, include and tuples. I think that these additions will make the language much more usable and yet increase implementation profile only moderately. 

- While the (cond)*(ifbr) + (1-cond)*(elsebr) expression with the short circuit is cool, it becomes much less usable if the expression is typechecked since it forces ifbr and elsebr to be of type int

- include is very simple to get running if you just "merge" into one AST before type checking, but makes this future poor language feel more complete by allowing common functions to be included.

- Of course one can get something like a list to work with functions only (see test/cases/list.intlang) but it will also quickly become a problem with the type checker that does not support Rank-2 Polymorphism. So thats why there are tuples now :)

As of now it has Lexer, Parser, Interpreter, Typechecker and some Tests :)

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
```
include mod
let answers = \x. if mod x 2 then [6,7] else [4,2] end;
(answer 3).1
>>> 
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

Ah lets remember the good old times one more time:

> - Say "if a>5 then 99 else 111" can be written as: `(a>5)*99 + (1-(a>5))*111`
> - I never said it is a nice language :|

intlang v2 does have if `if (a>5) then 99 else 111 end`. Note `else` and the `end` are not optional.

## let
- There are global definitions of the form: `let <id> = <lambda expression> ;`
- They are SSA but local lambda variables may shadow the global ones (and other lambda vars)
- All `let` are recursive
- The typechecker will attempt to make the `let` defs as polymorphic as possible

## Program Structure
- There may be some `include` statements followed by some `let` statements in the beginning of the program (all lets are recursive)
- It must end with an Expression (thinking of changing this, but it will stay for now)

## Types
- There are Ints `3`, Tuples `[1,2]` and of course function types.

## Vars
- Variable names must start with a Letter (not a Number)

## How to Run

- To build `dune build`
- To run some program (here fib.intlang): `dune exec bin/main.exe -- fib.intlang`
- To run the tests: `dune test`

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
- https://github.com/mgrabmueller/AlgorithmW
