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

let lib_tests = [
  (*mathlib.intlang test case here*)
  (*vectorlib_1d_i32.intlang test case here*)
]



let tests = [
  ("IOBasic", iobasic_tests);
  ("LangBasic", langbasic_tests);
  ("IOLib", iolib_tests);
  ("Lang", lang_tests);
]