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


## data analysis does not recognize tupuwrp promotion in "obvious" case

```
  bb8 "tco_loop_header" (%57, %58o):
    %62 = bopi32 lti32 %57 %42
    cbr %62 bb9 bb10

  bb9 "then" ():
    %65o = calldirect @135 %58!
    %124 %125b = tupborr %65
    %69 %70b = tupborr %125
    %73 = calldirect @108 %69
    %75 = immi8 44
    %77 = calldirect @1 %75
    %78 = immi32 1
    %79 = bopi32 addi32 %57 %78
    %126o = copy %70c
    drop %65
    br bb8(%79 %126!)
```
