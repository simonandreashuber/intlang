# intlang v4

A small eager functional language with 
- Immutable N dimensional vectors accelerated by a functional but in place optimization
- No Garbage Collector just compile time known allocation and freeing manged by the compiler
- N-ary Tuples
- `i32` and `i8` basic data types
- A stdlib with basics like IO or typical functional programming helpers like `map` or `left_fold`

## How to Run

- To build `dune build`
- To run some program (here fib.intlang): `dune exec bin/main.exe -- fib.intlang`
- To run the tests: `dune test`

## Dependencies
- ocaml 5.4.0 (what I developed on)
- LLVM 19 (also respective opam package) with clang-19 for compilation


## Informal Language Description

This is an informal but hopefully useful guide to the language.

### Basic Shape Of A Program

A file is a sequence of includes and top-level bindings (`include`, `let`, `let rec` and `and`):

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


### Expressions

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

Application is eager and left to right ie. given some Application a b first a is evaluated to eval(a) then b is evaluated to eval(b) and finally eval(b) is applied to eval(a).

### Functions

Functions are first-class, curried and application is left-associative.

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

The special form `\() expr` is used for a lambda that expects the unit type as its argument.

### Literals And Small Values

#### Integers

Integer literals are decimal `i32` values:

```intlang
let x = 123
```

Negative numbers are not separate literals. `-3` is parsed as unary minus applied to `3`.

#### Characters

`i8` literals are single-quoted characters:

```intlang
let a = 'a'
let nl = '\n'
let hex = '\x41'
```

The lexer accepts the usual escapes such as `\n`, `\t`, `\r`, `\\`, `\'`, `\"`, and hex escapes like `\xFF`.

#### Unit

Unit is written as `()`.

### Tuples

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

### Vectors

Vectors are memory contiguous sequences of values, with dynamic size, where all contained values are of the exact same type. Vectors are immutable just like the rest of the language. Vectors can contain other vectors (multidimensional vectors), `i32` or `i8`.

All indices in vector operations are evaluated eagerly left to right and each one must be an `i32`.

#### Vector Literals

```intlang
let v = vec['\x00', '\x01', '\x02']
let empty = vec[]
```

Strings are lexed as vectors of `i8`, so string literals behave like `i8` vectors. The empty string is treated specially and becomes an empty `i8` vector.

#### Vector Construction

`vecmk` creates vectors by repeating a default value:

```intlang
let filled = vecmk['\xaa', 5]
```

Multiple size arguments are allowed, so `vecmk` can build nested vectors too.

#### Vector Queries

`veclen[v]` returns the length of a vector.

`vecget[v, i, j, ...]` indexes into nested vectors.

`vecget[v]` is accepted and simply returns the vector unchanged.

#### Vector Updates

`vecset[v, value, i, j, ...]` returns a fresh vector with the given element updated (nesting also possible).

```intlang
let v2 = vecset[v, 99, 3]
```

#### Slicing And Extending

`vecslice[v, start, len]` returns a slice of the vector.

`vecextend[v, lit, off]` extends the vector with a fill value. Positive offsets append, negative offsets prepend.

### Operators

Intlang has separate operator families for `i32` and `i8`.

#### i32 Bops and Uops

```intlang
==  !=  <  >  <=  >=
<u  >u  <=u  >=u
+  -  *  /  %
/u  %u
&  |  ^
<<  >>  >>u
~
```
- All `i32` Bops and Uops return `i32`
- Unary `-` and `~` are unary operators.
- Shifts must be between `0` and `31`.
- Unsigned operations (trailing u) use the 32-bit unsigned versions of the underlying integer operations.

#### i8 Bops and Uops

```intlang
==i8  !=i8  <i8  >i8  <=i8  >=i8
+i8  -i8
&i8  |i8  ^i8
~i8
```

- Comparisons return `i32` results, all other Bops and Uops `i8` type.
- Unary `-i8` and `~i8` are unary operators.


### If

`if` expects an `i32` condition:

```intlang
if n = 0 then 1 else 0 end
```

Zero means false, any nonzero value means true.


### Includes

Includes are top-level only.

```intlang
include io
include "includetest"
```

There are two forms:

- `include name` for global-style includes
- `include "path"` for relative includes

Included modules/files are used through a module prefix, for example `io.writei8`, `math.gcd`, or `vector.veclen`.

The realtive includes require a path from the including file dir to the to be included file. The name used as module prefix for relative includes is the stem of the included .intlang file ie. top level expressions from `include "subfldr/somecode"` can be used like `somecode.sometoplvlexpression` if `sometoplvlexpression` is a top level expression in `./subfldr/somecode.intlang`.

Global includes just include from the intlang std library.

Both from leave out the .intlang for the module prefix.

### Typing

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

Top-level bindings can be polymorphic. That is why helpers like `map`, `left_fold`, and many library functions can stay generic.

### Builtins

The core builtins are:

- `readi8 : unit -> i8`
- `writei8 : i8 -> unit`
- `flush : unit -> unit`
- `i32_to_i8 : i32 -> i8`
- `i8_to_i32 : i8 -> i32`

### Examples

The code in `test/cases/*` and std lib `test/intlangstdlib/*` is a good source of inspiration. In `test/tests.ml` is the reference / "ground truth" on which the test are based.


## Compiler Pipeline

This is almost complete list of all the passes/steps the compiler takes to lower and optimize.

```
Lexer => 
Parser => 
Include Resolution => 
Recursive Check (all `let rec` bind to a lambda expression) =>
Typecheck =>
Monomorphization (eliminates Polymorphic types) => 
Vector Check (Vectors can only be of type Vector `i32` or `i8`) =>
MIR Generation =>
MIR Closure Call Elimination =>
MIR Tail Call Optimization =>
MIR Dead Code Elimination =>
MIR CFG Compaction =>
MIR BB Argument Borrowing Promotion =>
MIR Consumption (FBIP) and Function Monomorphization =>
MIR Drop Inserting =>
MIR Dead Function Elimination => 
LLVM Generation =>
LLVM to Binary with clang
```