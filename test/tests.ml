type test_case = {
  testname   : string;
  filename   : string;
  iterations : int;
  generator  : int -> (string * string);
}

(*
  ==== HELPER FOR THE GENERATOR FUNCTIONS ====
*)

(*string of int mod 256*)
let sim256 (i : int) : string = String.make 1 (char_of_int (Int.logand i 0xFF))
let strofint32 (i : int32) : string = Int32.to_string i ^ "\n"

let idx_to_randi32 seed i =
  let open Int32 in
  let x = logxor (of_int i) seed in
  let x = mul (logxor x (shift_right_logical x 16)) 0x45d9f3bl in
  let x = mul (logxor x (shift_right_logical x 16)) 0x45d9f3bl in
  let x = logxor x (shift_right_logical x 16) in
  x

let rand_printable_string maxlen seed =
  let state = Random.State.make [| seed |] in
  (* Generates a semi-random length between 1 and 64 *)
  let length = 1 + Random.State.int state maxlen in
  String.init length (fun _ ->
    (* 32 to 126 is the printable ASCII range (excludes all line breaks) *)
    Char.chr (32 + Random.State.int state 95)
  )

let int32csv xs = String.concat "," (List.map Int32.to_string xs) ^ "\n"
let int32csv_trailing xs = String.concat "," (List.map Int32.to_string xs) ^ ",\n"

let ranged_int32 seed min max i =
  Int32.add (Int32.unsigned_rem (idx_to_randi32 seed i) (Int32.sub max min)) min

let ranged_int32vec seed range maxlen i =
  let seed0 = idx_to_randi32 seed i in
  let len = 1 + Int32.to_int (Int32.unsigned_rem (idx_to_randi32 seed0 0) maxlen) in
  List.init len (fun j -> ranged_int32 seed0 (Int32.neg range) range (j+1))

(*
  ==== IOBasic Tests ====
  Theses test all the builtin functions, including
  basic i8 IO and i8 to i32 and reverse casting.
  They from the very most basic Tests on which all other Tests rely.
  As with the LangBasic Test these also rely on some language futures
  that are just needed even for the minimal program (see LangBasic Tests
  for a detailed description)
*)
let iobasic_tests = [
  {
    testname = "write_i8_const";
    filename = "cases/write_i8_const.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0x42));
  };
  {
    testname = "read_write_i8";
    filename = "cases/read_write_i8.intlang";
    iterations = 256;
    generator = (fun i -> let c = sim256 i in (c, c));
  };
  {
    testname = "builtin_casts";
    filename = "cases/builtin_casts.intlang";
    iterations = 256;
    generator = (fun i -> let c = sim256 i in (c, c));
  }
]

(*
  ==== Language Basic Tests ====
  These test the basic futures of the Language while only relying on
  the (IOBasic Tests). Some futures are so essential to a minimal program
  that they cant really be tested in isolation. These include: Application,
  Unit Lambda, Unit Literal, Sequence and the top level main let binding.
  Before these test the IOBasic tests should run since these test rely on 
  the builtin IO and Cast functions
*)
let langbasic_tests = [
  {
    testname = "lit_i8";
    filename = "cases/lit_i8.intlang";
    iterations = 1;
    generator = (fun _ -> ("", String.concat "" (List.init 256 (fun i -> sim256 i))));
  };
  {
    testname = "lit_i32_basic";
    filename = "cases/lit_i32_basic.intlang";
    iterations = 1;
    generator = (fun _ -> ("", String.concat "" (List.init 256 (fun i -> sim256 i))));
  };
  {
  testname = "letin";
  filename = "cases/letin.intlang";
  iterations = 32;
  generator = (fun i -> (sim256 (i*6) ^ sim256 (i*7), sim256 (i*7)));
  };
  {
    testname = "lam";
    filename = "cases/lam.intlang";
    iterations = 32;
    generator = (fun i -> (sim256 (i*8) ^ sim256 (i*9), sim256 (i*9)));
  };
  {
    testname = "ifelse";
    filename = "cases/ifelse.intlang";
    iterations = 32;
    generator = (fun i ->
      let c = sim256 (i * 7 + 3) and et = sim256 (i * 11 + 5) and ef = sim256 (i * 13 + 7) in
      (c ^ et ^ ef, if c = sim256 0 then ef else et));
  };
  {
    testname = "uop_i8";
    filename = "cases/uop_i8.intlang";
    iterations = 256;
    generator = (fun i -> (sim256 i, sim256 (Int.lognot i) ^ sim256 (256 - i)));
  };
  {
    testname = "bop_i8";
    filename = "cases/bop_i8.intlang";
    iterations = 256 * 256; (*dont even think about running this in separate mode it takes forever *)
    generator = (fun i -> let (i0, i1) = (i / 256, i mod 256) in
      (sim256 i0 ^ sim256 i1,
        sim256 (if i0 = i1 then 1 else 0) ^ 
        sim256 (if i0 <> i1 then 1 else 0) ^
        sim256 (if i0 < i1 then 1 else 0) ^
        sim256 (if i0 > i1 then 1 else 0) ^
        sim256 (if i0 <= i1 then 1 else 0) ^
        sim256 (if i0 >= i1 then 1 else 0) ^
        sim256 ((i0 + i1) mod 256) ^
        sim256 ((256 + i0 - i1) mod 256) ^
        sim256 (Int.logand i0 i1) ^
        sim256 (Int.logor i0 i1) ^
        sim256 (Int.logxor i0 i1)
      ));
  };
  {
    testname = "uop_bop_i32_basic";
    filename = "cases/uop_bop_i32_basic.intlang";
    iterations = 1;
    generator = (fun _ ->
      ("",
        sim256 0 ^
        sim256 1 ^
        sim256 0 ^
        sim256 1 ^
        sim256 1 ^
        sim256 0 ^
        sim256 1 ^
        sim256 1 ^
        sim256 13 ^
        sim256 2 ^
        sim256 0 ^
        sim256 12 ^
        sim256 20 ^
        sim256 1 ^
        sim256 3 ^
        sim256 0));
  };
  {
    testname = "letrecin";
    filename = "cases/letrecin.intlang";
    iterations = 13;
    generator = (fun i -> 
                  let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2) in
                (sim256 i, sim256 (fib i))
                );
  };
  {
    testname = "tuple";
    filename = "cases/tuple.intlang";
    iterations = 32;
    generator = (fun i -> 
                  let inout = sim256 i ^ sim256 (i+1) ^ sim256 (i+2) ^ sim256 (i+3) in
                  (inout, inout);
                );
  };
  {
    testname = "veclit_1d";
    filename = "cases/veclit_1d.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0 ^ sim256 1 ^ sim256 2 ^ sim256 3 ^ sim256 4));
  };
  {
    testname = "vecmk_1d";
    filename = "cases/vecmk_1d.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0xAA ^ sim256 0xAA ^ sim256 0xAA ^ sim256 0xAA ^ sim256 0xAA));
  };
  {
    testname = "vecset_1d";
    filename = "cases/vecset_1d.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0xAA ^ sim256 0xAA ^ sim256 0xAA ^ sim256 0x30 ^ sim256 0xAA));
  };
  {
    testname = "vecresz_1d";
    filename = "cases/vecresz_1d.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0xFF ^ sim256 1 ^ sim256 2 ^ sim256 3 ^ sim256 5));
  };
  {
    testname = "let";
    filename = "cases/let.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 10 ^ sim256 11));
  };
  {
    testname = "letrec";
    filename = "cases/letrec.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 10 ^ sim256 20));
  };
  {
    testname = "letrecand";
    filename = "cases/letrecand.intlang";
    iterations = 32;
    generator = (fun i ->
      let rec collatz n =
        if n = 1 then 0
        else 1 + collatz (if n mod 2 = 0 then n / 2 else 3 * n + 1)
      in
      let n = ((i * 7 + 1) mod 255) + 1 in
      (sim256 n, sim256 (collatz n)));
  };
]

(*
  ==== IOLib tests ====
  Test the input / output library (io.intlang).
  Critical as all test (exluding IOBasic and LangBasic tests)
  rely on a correctly working IO Library.
  Potential Extensions: Test read_str and write_str
*)
let iolib_tests = [
  {
    testname = "read_write_i32_positive";
    filename = "cases/read_write_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let max_i32 = Int32.to_int Int32.max_int in
      let i32 =  Int.shift_right max_i32 i in
      let i32str = string_of_int (i32) ^ "\n" in
      (i32str, i32str)
      );
  };
  {
    testname = "read_write_i32_negative";
    filename = "cases/read_write_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let min_i32 = Int32.to_int Int32.min_int in
      let i32 =  Int.shift_right min_i32 i in
      let i32str = string_of_int (i32) ^ "\n" in
      (i32str, i32str)
      );
  };
  {
    testname = "read_write_i32_rand";
    filename = "cases/read_write_i32.intlang";
    iterations = 1024;
    generator = (fun i ->
      let i32 =  idx_to_randi32 2026070511l i in
      let i32str = strofint32 i32 in
      (i32str, i32str)
    );
  };
  {
    testname = "read_write_ln";
    filename = "cases/read_write_ln.intlang";
    iterations = 256;
    generator = (fun i ->
      let rln = rand_printable_string 900 i ^ "\n" in
      (rln, rln)
    );
  };
  {
    testname = "read_write_csi32_rand";
    filename = "cases/read_write_csi32.intlang";
    iterations = 14;
    generator = (fun i ->
      let csi32 = String.concat "," (List.init (6*i + 1) (fun j ->  Int32.to_string (idx_to_randi32 2026070611l (j + (3*i*(i-1)))))) ^ "\n" in
      (csi32, csi32)
    );
  };
  {
    testname = "read_write_csi32_edge";
    filename = "cases/read_write_csi32.intlang";
    iterations = 4;
    generator = (fun i ->
      let csi32 = 
      if i = 0 then
        "0,0,0,0,0,0\n" (*minimal string len => maximal vec len*)
      else if i = 1 then
        "-2147483648,-2147483648,-2147483648,-2147483648,-2147483648,-2147483648\n" (*max str len => minimal vec len*)
      else if i = 2 then
        "0\n" (*minimal string len => maximal vec len*)
      else
        "-2147483645,-2147483645,-2147483645,-2147483645,-2147483645,-2147483645\n" (*max str len => minimal vec len*)
      in
      (csi32, csi32)
    );
  };
]

(*
  ==== Language Tests ====
  These test the remaining Language Futures that can not be tested
  to an acceptable extend without relying on the io lib.
*)

let lang_tests = [
  {
    testname = "uop_i32";
    filename = "cases/uop_i32.intlang";
    iterations = 256;
    generator = (fun i ->
      let x = idx_to_randi32 2026070512l i in
      let input = strofint32 x in
      let expected =
        strofint32 (Int32.neg x) ^
        strofint32 (Int32.lognot x)
      in
      (input, expected));
  };
  {
    testname = "bop_i32";
    filename = "cases/bop_i32.intlang";
    iterations = 256;
    generator = (fun i ->
      let x = idx_to_randi32 2026070513l i in
      let y = idx_to_randi32 150706202l i in
      let y_nonzero = if y = 0l then 1l else y in
      let y_shift = Int32.to_int @@ Int32.unsigned_rem y 32l in
      let b32 b = if b then 1l else 0l in
      let input = strofint32 x ^ strofint32 y in
      let expected =
        strofint32 (b32 (x = y)) ^
        strofint32 (b32 (x <> y)) ^
        strofint32 (b32 (x < y)) ^ 
        strofint32 (b32 (x > y)) ^
        strofint32 (b32 (x <= y)) ^
        strofint32 (b32 (x >= y)) ^
        strofint32 (b32 (Int32.unsigned_compare x y < 0)) ^
        strofint32 (b32 (Int32.unsigned_compare x y > 0)) ^
        strofint32 (b32 (Int32.unsigned_compare x y <= 0)) ^
        strofint32 (b32 (Int32.unsigned_compare x y >= 0)) ^
        strofint32 (Int32.mul x y) ^
        strofint32 (Int32.sub x y) ^
        strofint32 (Int32.add x y) ^
        strofint32 (Int32.div x y_nonzero) ^
        strofint32 (Int32.rem x y_nonzero) ^
        strofint32 (Int32.unsigned_div x y_nonzero) ^
        strofint32 (Int32.unsigned_rem x y_nonzero) ^
        strofint32 (Int32.logand x y) ^
        strofint32 (Int32.logor x y) ^
        strofint32 (Int32.logxor x y) ^
        strofint32 (Int32.shift_left x y_shift) ^
        strofint32 (Int32.shift_right x y_shift) ^
        strofint32 (Int32.shift_right_logical x y_shift)
      in
      (input, expected));
  };
]

(*
  ==== Lib Tests ====
  These test test the intlangstdlib
*)

let isqrt_i32 x =
  if x < 0l then raise (Invalid_argument "isqrt: negative argument");
  Int32.of_float (sqrt (Int32.to_float x))

let pow_i32 base exp = Int32.of_float ((Int32.to_float base) ** (Int32.to_float exp))

let gcd_i32 a b =
  let rec loop x y =
    if y = 0l then x else loop y (Int32.rem x y)
  in
  loop a b

let cmp_i32 a b =
  if Int32.compare a b < 0 then -1l else if Int32.compare a b > 0 then 1l else 0l

let inbounds_i32 low high value =
  if Int32.compare low value <= 0 && Int32.compare value high < 0 then 1l else 0l

let cmp_i32_lists lst0 lst1 =
  let elcmp = (fun x y -> Int32.to_int @@ cmp_i32 x y) in
  if List.compare elcmp lst0 lst1 < 0 then -1l else if List.compare elcmp lst0 lst1 > 0 then 1l else 0l

let lib_tests = [
  {
    testname = "mathlib";
    filename = "cases/mathlib.intlang";
    iterations = 32;
    generator = (fun i ->
      let sqrt_input = ranged_int32 2026070701l 0l 10000l i in
      let pow_base = ranged_int32 2026070702l (-100l) 100l i in
      let pow_exp = ranged_int32 2026070703l 1l 4l i in
      let gcd_a = ranged_int32 2026070704l 0l 100000l i in
      let gcd_b = ranged_int32 2026070705l 0l 100000l i in
      let cmp_a = idx_to_randi32 2026070706l i in
      let cmp_b = idx_to_randi32 2026070707l i in
      let low = idx_to_randi32 2026070708l i in
      let value = idx_to_randi32 2026070709l i in
      let high = idx_to_randi32 2026070710l i in
      let input = int32csv [sqrt_input; pow_base; pow_exp; gcd_a; gcd_b; cmp_a; cmp_b; low; high; value] in
      let expected = String.concat "" [
        strofint32 (isqrt_i32 sqrt_input);
        strofint32 (pow_i32 pow_base pow_exp);
        strofint32 (gcd_i32 gcd_a gcd_b);
        strofint32 (cmp_i32 cmp_a cmp_b);
        strofint32 (inbounds_i32 low high value)
      ] in
      (input, expected));
  };
  {
    testname = "vectorlib_1d_i32";
    filename = "cases/vectorlib_1d_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let vi32 = ranged_int32vec 2026070711l 1000l 32l i in
      let double = List.map (Int32.mul 2l) vi32 in
      let copied = vi32 @ vi32 in
      let addreduce_left = List.fold_left Int32.add 0l vi32 in
      let addreduce_right = List.fold_right Int32.add vi32 0l in
      let middle = (List.length vi32) / 2 in
      let vi32larger = List.mapi (fun idx value -> if idx = middle then Int32.add value 1l else value) vi32 in
      let input = int32csv vi32 in
      let expected = String.concat "" [
        int32csv double;
        int32csv copied;
        strofint32 addreduce_left;
        strofint32 addreduce_right;
        strofint32 (cmp_i32_lists vi32 vi32);
        strofint32 (cmp_i32_lists vi32 vi32larger);
        strofint32 (cmp_i32_lists vi32larger vi32);
        strofint32 (cmp_i32_lists vi32 copied);
        strofint32 (cmp_i32_lists copied vi32);
        int32csv_trailing vi32
      ] in
      (input, expected));
  };
]



let tests = [
  ("IOBasic", iobasic_tests);
  ("LangBasic", langbasic_tests);
  ("IOLib", iolib_tests);
  ("Lang", lang_tests);
  ("Lib", lib_tests);
]