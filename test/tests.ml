type test_case = {
  testname   : string;
  filename   : string;
  iterations : int;
  generator  : int -> (string * string);
}

(*some helpers for the generators*)
(*string of int mod 256*)
let sim256 (i : int) : string = String.make 1 (char_of_int (Int.logand i 0xFF))

let basicIO_tests = [
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

let basicLang_tests = [
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
    iterations = 256 * 256;
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

let strlib_tests = [
  {
    testname = "read_write_i32_positive";
    filename = "cases/read_write_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let max_i32 = Int32.to_int Int32.max_int in
      let i32 =  Int.shift_right max_i32 i in
      (string_of_int (i32) ^ "\n", string_of_int (i32) ^ "\n")
    );
  };
  {
    testname = "read_write_i32_negative";
    filename = "cases/read_write_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let min_i32 = Int32.to_int Int32.min_int in
      let i32 =  Int.shift_right min_i32 i in
      (string_of_int (i32) ^ "\n", string_of_int (i32) ^ "\n")
    );
  }
]

let tests = [
  ("basicIO", basicIO_tests);
  ("basicLang", basicLang_tests);
  ("strlib", strlib_tests)
]