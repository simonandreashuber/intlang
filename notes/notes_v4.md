## TODO 
	- Fix test cases and stdlib (especially the let rec things)
	- Notes after coding a bit:
		- if u have unsigned ops mb u should have an unsigned literal
	- \() => [i8]. unit lambda annotation might be nice
	- typecheker: give each constraint an error string attachment for better error messages
	- vecmk should be callable with a type not only with a defval
	- mb dont include _sometoplevelname, kinda as a means to private functions...
	- add pass after the mono to check that there are no vectors of functions

	- BUG: On include, if in the included file there is some name that is refering to the own file has the file tag in the front this does seem to work
		   ie. io.write_ln does work in io.intlang as long as io.intlang is only included and not run on its own

	- if left to right logic shortcut??

## Test Suite Should Have
 - need to add read i32 vector to str lib
 - test all bop and uops, input two i32 then output an int with results from all the uops and bops that exist
 - to test specific basic language futures we need to write the test without str lib as it uses almost all language futures
	- to get these test I think I should start at the io then the cast builtins
	- then go through the ast and just try to write test that test as little as possible (if some future is needed additionally try to test the additional future first)
 - if else
 - builtin casts

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
