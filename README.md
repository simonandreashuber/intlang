# intlang v4

## TODO 
	- Fix test cases and stdlib (especially the let rec things)
	- Notes after coding a bit:
		- if u have unsigned ops mb u should have an unsigned literal
## Futures

The idea is to make the language more usable/futurefull and faster.

- [] N-arry tuples
	- [] (a,b,c)
	- [] let (a,b, \_) = tup in
	- [] optional: \ (a,b,\_). Should be possible with just syntactic sugar
	- [] optional: mutli lambda \ a b c.
- [] new basic data type i8
	- [] the normal ints now become i32
		- I always liked to know what the machine is doing and this follows design philosophy 
	- [] ASCII
	- [] then str is \[i8\]
	- [] Explicit casting functions: i32_to_i8 and i8_to_i32
	- [] ==i8 !=i8 <i8 <=i8 >i8 >=i8
	- [] +i8 -i8
	- [] &i8 |i8 ^i8 ~i8
	- [] 'c' or '\xff' init format for i8
	- [] "string" shorthand for vec\['s', 't', 'r', 'i', 'n', 'g', '\n' \]
 - [] Find new name for language since there are not only "ints" anymore (not true in a way there are now just two sizes of ints sooooo)
	- mb: Gepard 
- [] IO
	- [] unit type written as ()
	- [] \\() for function with unit type
	- [] readstrln with unit -> \[i8\] type
	- [] writestr with \[i8\] -> unit type
	- [] flush with unit -> unit type
- [] lambda type annotations 
	- \ x : typx.
	- \ x : typx => typout.
	- int: i32
	- char: i8
	- tupel: typa * typb
	- vectors: \[typa\] where typa is a vector or an int
	- functions: typa -> typb
- [] New operators for i32
	- [] > >= =< !=
 	- [] >u <u >=u <=u /u >>>
	- [] << >> | & % ^
	- [] - ~
- [] let rec, and
	- only allow letrec for function types
	- just makes more sense in an eager language
	- the scc is kinda neat but also potentially rearranges things
	- I think in general I like languages where the programmer knows what is gona happen in the compiler and the scc goes a bit against this
- [] intlangstdlib path machine independent
	- [] relative includes are not just ascii strings so need to endforce id at the end
- [] Do strictness check pass
- [] lower via MIR to avoid copies 
	- [] MIR def (still need to figure out specification)
	- [] Dataflow Analysis
	- [] Copy Optimization (Functional but in Place)
	- [] Mb: DCE
- [] GC with ARC
- [] new vectors
	- type determined by inntertype of char or int and dimension/depth
	- for index lists allow it to be empty or just some partial access
	- [] vecget\[oldv, index list\]
	- [] vecset\[oldv, newval,  index list\]
		- optional: should I allow a index list shorter than the vectors dim so we can get and set a vector
	- [] vecmk\[innerval, size list\]
		- size list determines the dim of the vector
	- [] vec\[list of vectors, ints or chars\]
	- [] vecresz\[oldv, defval, newstart, newend\]
		- resize the vector oldv
			- newstart is an i32 value (signed). If positive then then start of the 
			  vector is pushed into the old vector by that amount. If negative the vector is extended
			  by the absolute value with prepending
			- newend is an i32 value (signed). If negative then then end of the 
		      vector is pushed into the old vector by that amount. If positive the vector is extended
			  by the absolute value with appending
			- All extension fills the new slots with defval
			- In other words image the old vector is embedded in an infinite sequence of defval
			  then the newstart and newend are the offsets used at the start and end of the embedded 
			  vector to create the new vector
	- [] optinal: write tiling
		- the idea is to split some big matrix that needs to be written to into tiles but keep the original memory layout
		- would have to prove at compile time what values tilefun can attain
			- prohibit capturing for tilefun is probably a good restriction
		- if the tiled computation never needs a copy then vecasmbl can just make the final result appear as one big vector again at 0 cost
		- I have put this as optional as I feel like it is not going to be simple but would be so cool :)
			```
			-- tilefun gives int in 0, 1, 2, 3 for each index
			-- determines which entry of the tuple owns which part of bigmatC
			let tilefun = \idx. \vsize. 
				let rowidx = idx % vsize in
				let cloidx = idx / vsize in
				let halfsize = vsize / 2 in
				(rowidx > halfsize) + 2 * (colidx > halfsize);
			let (ur, ul, lr, ll) = vectile\[bigmatC, tilefun\] in
			-- do some tiled computation here like a tiled matmul
			-- where we now have nur, nul, nlr, nll
			let newv = vecasmbl[(nur, nul, nlr, nll)]
			```


A small functional language called intlang, that I came up with (wow super original, I know) to get some experience with writing a Parser. This is about trying and probably getting a bunch of things wrong but learn from the mistakes. The project a evolved a bit past what I originally imagined. 

As of now it has Lexer, Parser, Include Resolver, Typechecker, Monomorphization, Interpreter, Lowering to LLVM and some Tests :)

## Formal Semantics and Type System

in `doc/formal-semantics-and-type-system.tex` there are notes on:
 - Lexical Conventions
 - Concrete Grammar (BNF)
 - Type System
 - Evaluation Strategy

## Syntax by Example

in `/test/cases` and `/test/intlangstdlib` there are plenty of examples. Here are also some

```
include vector
let v = vec[1,2,3,4,5,6,7,8,9];
let vsum = vector.vec_left_fold (\acc. \x. acc+x) 0 v;
vsum
>>> 45
```

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

## Dependencies (not complete)

- LLVM 19 (also respective ocaml package) with clang for compilation

## Inspirations
- https://github.com/twolodzko/twolodzko.github.io/tree/main/snippets/ocaml-parser
- https://github.com/mgrabmueller/AlgorithmW
