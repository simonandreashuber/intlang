# Intlang

This is an informal but hopefully useful guide to the language. It is written from the point of view of how the parser, typechecker, interpreter, and tests actually behave.

## What Intlang Is

Intlang is an eager, functional language with explicit `unit`, 4 and 1 byte wide integer (`i32` and `i8`), tuples, and vectors. Functions are first-class, application is left-associative, and top-level programs are built from `include`, `let`, `let rec` and `and` bindings.

There is no separate boolean type. Conditions and comparisons use `i32`, where `0` means false and any nonzero value means true.

## Basic Shape Of A Program

A file is a sequence of includes and top-level bindings:

```intlang
include io
include "math"

let x = 3
let rec fib = \n. if n < 2 then n else fib (n - 1) + fib (n - 2) end
let main = \() io.writeln_i32 (fib x)
```

Top-level bindings are either:

- `let name = expr`
- `let rec name = expr`
- `let rec name = expr and other = expr and ...`

If a `main` binding exists, it should have type `unit -> unit`.

Comments start with `--` and run to the end of the line.



## Expressions

Intlang is expression-based. You can nest expressions freely, and sequencing uses `;` (the left side of `;` needs to have unit type):

```intlang
let main = \() (
	writei8 'a';
	writei8 'b';
	flush ()
)
```

Useful expression forms are:

- variables: `x`
- application: `f x y`
- lambdas: `\x. expr`
- unit lambdas: `\() expr`
- `if cond then a else b end`
- local binding: `let x = e in body`
- recursive local binding: `let rec f = e in body`
- tuple binding: `let (a, b) = e in body`
- tuple values: `(a, b, c)`
- vector literals: `vec[1, 2, 3]`
- vector constructors and accessors: `vecmk[...]`, `veclen[...]`, `vecget[...]`, `vecset[...]`, `vecslice[...]`, `vecextend[...]`

Application is eager: function arguments are evaluated before the call happens.
## Functions

Functions are curried by default. Multiple parameters are just nested lambdas written in a compact form:

```intlang
let add = \x y. x + y
let add5 = add 5
```

Typed parameters are optional:

```intlang
let inc = \x : i32. x + 1
let first = \(x : i32) (y : i32). x
```

You can also annotate the final result with `=>`:

```intlang
let id_i32 = \x : i32 => i32. x
```

The special form `\() expr` is used for nullary functions. That is the common shape of `main`.

## Literals And Small Values

### Integers

Integer literals are decimal `i32` values:

```intlang
let x = 123
```

Negative numbers are not separate literals. `-3` is parsed as unary minus applied to `3`.

### Characters

`i8` literals are single-quoted characters:

```intlang
let a = 'a'
let nl = '\n'
let hex = '\x41'
```

The lexer accepts the usual escapes such as `\n`, `\t`, `\r`, `\\`, `\'`, `\"`, and hex escapes like `\xFF`.

### Unit

Unit is written as `()`.

## Tuples

Tuples are product values written with commas:

```intlang
let pair = (1, 'a')
let triple = (1, 2, 3)
```

A single parenthesized expression is just grouping, not a one-element tuple. Tuple values require at least two elements.

Tuples can only be destructed with a let in:

```intlang
let (x, y) = pair in
x + 1
```

Tuple patterns can use `_` for values you want to ignore.

Tuple types use `*`:

```intlang
let swap = \p : i32 * i8. let (x, y) = p in (y, x)
```

## Vectors

Vectors are immutable.

### Vector Literals

```intlang
let v = vec['\x00', '\x01', '\x02']
let empty = vec[]
```

Strings are lexed as vectors of `i8`, so string literals behave like byte vectors. The empty string is treated specially and becomes an empty byte vector.

### Vector Construction

`vecmk` creates vectors by repeating a default value:

```intlang
let filled = vecmk['\xaa', 5]
```

Multiple size arguments are allowed, so `vecmk` can build nested vectors too.

### Vector Queries

`veclen[v]` returns the length of a vector.

`vecget[v, i, j, ...]` indexes into nested vectors. The indices are evaluated eagerly and each one must be an `i32`.

`vecget[v]` is accepted and simply returns the vector unchanged.

### Vector Updates

`vecset[v, value, i, j, ...]` returns a fresh vector with the given element updated (nesting also possible).

```intlang
let v2 = vecset[v, 99, 3]
```

### Slicing And Extending

`vecslice[v, start, len]` returns a slice of the vector.

`vecextend[v, lit, off]` extends the vector with a fill value. Positive offsets extend on the right, negative offsets extend on the left.

## Operators

Intlang has separate operator families for `i32` and `i8`.

### i32 Operators

```intlang
==  !=  <  >  <=  >=
<u  >u  <=u  >=u
+  -  *  /  %
/u  %u
&  |  ^
<<  >>  >>u
~
```

Unary `-` and `~` are the main i32 unary operators.

### i8 Operators

```intlang
==i8  !=i8  <i8  >i8  <=i8  >=i8
+i8  -i8
&i8  |i8  ^i8
~i8
```

Comparisons return `i32` results, not a boolean type.

### Runtime Notes

- Division and modulo by zero raise errors.
- Shifts must be between `0` and `31`.
- Unsigned operations use the 32-bit unsigned versions of the underlying integer operations.
- `i8` arithmetic wraps mod 256.

## Control Flow

`if` expects an `i32` condition:

```intlang
if n = 0 then 1 else 0 end
```

Zero means false, any nonzero value means true.

Sequence is written with `;` and is often used inside `main`:

```intlang
writei8 'a';
writei8 'b';
flush ()
```

## Includes

Includes are top-level only.

```intlang
include io
include "includetest"
```

There are two forms:

- `include name` for global/module-style includes
- `include "path"` for relative includes

Included modules are used through a module prefix, for example `io.writei8`, `math.gcd`, or `vector.veclen`.

The realtive includes require a path from the including files dir to the to be included file. Global includes just include from the intlang std library.

Both from leave out the .intlang.

## Typing

Here are some type examples

```intlang
i32
i8
unit
[i32]
i32 -> i32
i32 * i8
```

Function arrows are right-associative, so `i32 -> i32 -> i32` means `i32 -> (i32 -> i32)`.

Vectors are written as `[T]` in annotations. Tuples are writen as `T0 * T1`

The language is inferred where possible, but top-level bindings can be polymorphic. That is why helpers like `map`, `left_fold`, and many library functions can stay generic.

## Builtins

The core builtins are:

- `readi8 : unit -> i8`
- `writei8 : i8 -> unit`
- `flush : unit -> unit`
- `i32_to_i8 : i32 -> i8`
- `i8_to_i32 : i8 -> i32`

## How To Read The Test Suite

The tests `test/cases/` and std lib `test/intlangstdlib` are a good source of inspiration.

The general style is small, eager, curried functions with lots of composition and lightweight data encoding. Booleans are integers, strings are byte vectors, and recursive data is often represented with vectors or tuples.

