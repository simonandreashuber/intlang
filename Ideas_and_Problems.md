# Ideas and Problems

## Vecslice can only borrow

```
    %84b = vecslice %80 %81 %83
    %118 = immi32 0
    %119o = tupwrp %118 %84c
    drop %80 %71
    br bb14(%119!)
```

Classic pattern where unneeded copy is made. The solution is also simple, just extend the vector representaiton in llvm from {ptr, i32} to {ptr, ptr, i32}, where one of the pointers stays on the initially allocated region such that the free can happen correctly. Another approach would be to only allow this for slicing without an offset but this just seems to overcompilicate things for no real reason.


### Non fused vec ops can lead to unneeded copies

```
  bb4 "then" ():
    %20b = vecread %0 %1
    %24 = calldirect @111 %20 %2
    %25 = immi32 1
    %26 = uopi32 negi32 %25
    %27o = vecwrite %20c %26 %24
    %28o = vecinsert %0! %27! %1
    br bb6(%28!)  
```

### Missing call devirtualization in recursive funtions

Recursive function are generally not inlined. But this does also remove the primiry mechanism of call devirtualizazion that the compiler currently takes (inlining). An example would be dfsaux in the graph lib:

```
fn i32*vec<1,i32>*vec<1,i32>*vec<1,i32> @143 "dfsaux" (vec<2,i32> %0b "g", clos(vec<1,i32>->vec<1,i32>) %1b "ord", i32 %2 "p", i32*vec<1,i32>*vec<1,i32>*vec<1,i32> %3o "acc", i32 %4 "v") {
  bb0 "entry" ():
    %7 %8o %9o %10o = tupuwrp %3!
    %12 = vecread %8 %4
    %13 = immi32 0
    %14 = bopi32 gteqi32 %12 %13
    cbr %14 bb1 bb2

  bb1 "then" ():
    %15o = tupwrp %7 %8! %9! %10!
    br bb3(%15!)

  bb2 "else" ():
    %16o = vecwrite %8! %7 %4
    %17o = vecwrite %10! %2 %4
    %18 = immi32 1
    %19 = bopi32 addi32 %7 %18
    %20o = tupwrp %19 %16! %9! %17!
    %21b = vecread %0 %4
    %22o = pack %1c %21c
    %23o = callclosure %22!
    %45 = immi32 0
    br bb7(%20! %45)

  .....
```

%1 is the order closure for the dfs to know the traversal order on a given node. In this specific case the penalty is likely not very high since the order closure probably does not have large caputred data. But in general this could of course happen. What is needed here is a MIR Monomorpization.
