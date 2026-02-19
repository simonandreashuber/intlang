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
```

## Operators
- Only binary opertors: ==,<,*,-,+ (decreasing binding strength)
- Hint: Use 0-... to do negation

## Key"words"
- let, =, ;

## Types
- int
- function types

## Restirctions on vars
- only lowercase no other symbols no numbers

### What about if else
- Say "if a>5 then 99 else 111" can be written as: "(a>5)*99 + (1-(a>5))*111"
- I never said it is a nice language :||||||||||||||||

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
