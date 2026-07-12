# MIR

## Basics
- needs to be ssa
    - remember: This means that every value is defined exactly once, and every use of a value must be dominated by the definition.
- flattend linear cf
- 3-tuple instruction form

## Cranelift IR
- https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md
- uses BB paramters rather than phinodes seems similarly powerfull but cleaner in my book
- also are the ssa values isolated in bbs?
    - no v0 is input to entry bb and then used in bb3
    - [function preamble] which declares a number of entities that can be referenced inside the function. In the example above, the preamble declares a single explicit stack slot, ss0
- always terminator (no reason not to, as in asm it is more an artefact of the computer architecture)
- no ptr type just integer with ie. ptr is i64
- Instructions define zero, one, or more result values. 
- All SSA values are either BB parameters or instruction results.

