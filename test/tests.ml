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
let prln_int32 (i : int32) : string = Int32.to_string i ^ "\n"

let prln_int32lst xs = String.concat "," (List.map Int32.to_string xs) ^ "\n"

let prln_int32tuplst xs = let (vs, us) = List.split xs in prln_int32lst us ^ prln_int32lst vs

let prln_int32lst_trailing xs = String.concat "," (List.map Int32.to_string xs) ^ ",\n"

let prln_int32array arr = prln_int32lst (Array.to_list arr)

let rand_printable_str maxlen seed =
  let state = Random.State.make [| seed |] in
  (* Generates a semi-random length between 1 and 64 *)
  let length = 1 + Random.State.int state maxlen in
  String.init length (fun _ ->
    (* 32 to 126 is the printable ASCII range (excludes all line breaks) *)
    Char.chr (32 + Random.State.int state 95)
  )

let rand_int32 seed i =
  let open Int32 in
  let x = logxor (of_int i) seed in
  let x = mul (logxor x (shift_right_logical x 16)) 0x45d9f3bl in
  let x = mul (logxor x (shift_right_logical x 16)) 0x45d9f3bl in
  let x = logxor x (shift_right_logical x 16) in
  x

let rand_int32_ranged seed i min max =
  Int32.add (Int32.unsigned_rem (rand_int32 seed i) (Int32.max (Int32.sub max min) 1l)) min

let rand_int32lst_ranged seed i min max maxlen =
  let seed0 = rand_int32 seed i in
  let len = 1 + Int32.to_int (Int32.unsigned_rem (rand_int32 seed0 0) maxlen) in
  List.init len (fun j -> rand_int32_ranged seed0 (j+1) min max)

let rand_int32tuplst_ranged seed i min max maxlen =
  let seed0 = rand_int32 seed i in
  let len = 1 + Int32.to_int (Int32.unsigned_rem (rand_int32 seed0 0) maxlen) in
  List.init len (fun j -> (rand_int32_ranged seed0 (2*j) min max, rand_int32_ranged seed0 (2*j+1) min max))

let rand_int32array_ranged seed i min max len = 
  let seed0 = rand_int32 seed i in
  Array.init (Int32.to_int len) (fun j -> rand_int32_ranged seed0 j min max)


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
  {
    testname = "comments";
    filename = "cases/comments.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 0x55));
  };
  {
    testname = "includetest";
    filename = "cases/includetest.intlang";
    iterations = 1;
    generator = (fun _ -> ("", sim256 8));
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
      let i32 =  rand_int32 2026070511l i in
      let i32str = prln_int32 i32 in
      (i32str, i32str)
    );
  };
  {
    testname = "read_write_ln";
    filename = "cases/read_write_ln.intlang";
    iterations = 256;
    generator = (fun i ->
      let rln = rand_printable_str 900 i ^ "\n" in
      (rln, rln)
    );
  };
  {
    testname = "read_write_csi32_rand";
    filename = "cases/read_write_csi32.intlang";
    iterations = 14;
    generator = (fun i ->
      let csi32 = String.concat "," (List.init (6*i + 1) (fun j ->  Int32.to_string (rand_int32 2026070611l (j + (3*i*(i-1)))))) ^ "\n" in
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
      let x = rand_int32 2026070512l i in
      let input = prln_int32 x in
      let expected =
        prln_int32 (Int32.neg x) ^
        prln_int32 (Int32.lognot x)
      in
      (input, expected));
  };
  {
    testname = "bop_i32";
    filename = "cases/bop_i32.intlang";
    iterations = 256;
    generator = (fun i ->
      let x = rand_int32 2026070513l i in
      let y = rand_int32 150706202l i in
      let y_nonzero = if y = 0l then 1l else y in
      let y_shift = Int32.to_int @@ Int32.unsigned_rem y 32l in
      let b32 b = if b then 1l else 0l in
      let input = prln_int32 x ^ prln_int32 y in
      let expected =
        prln_int32 (b32 (x = y)) ^
        prln_int32 (b32 (x <> y)) ^
        prln_int32 (b32 (x < y)) ^ 
        prln_int32 (b32 (x > y)) ^
        prln_int32 (b32 (x <= y)) ^
        prln_int32 (b32 (x >= y)) ^
        prln_int32 (b32 (Int32.unsigned_compare x y < 0)) ^
        prln_int32 (b32 (Int32.unsigned_compare x y > 0)) ^
        prln_int32 (b32 (Int32.unsigned_compare x y <= 0)) ^
        prln_int32 (b32 (Int32.unsigned_compare x y >= 0)) ^
        prln_int32 (Int32.mul x y) ^
        prln_int32 (Int32.sub x y) ^
        prln_int32 (Int32.add x y) ^
        prln_int32 (Int32.div x y_nonzero) ^
        prln_int32 (Int32.rem x y_nonzero) ^
        prln_int32 (Int32.unsigned_div x y_nonzero) ^
        prln_int32 (Int32.unsigned_rem x y_nonzero) ^
        prln_int32 (Int32.logand x y) ^
        prln_int32 (Int32.logor x y) ^
        prln_int32 (Int32.logxor x y) ^
        prln_int32 (Int32.shift_left x y_shift) ^
        prln_int32 (Int32.shift_right x y_shift) ^
        prln_int32 (Int32.shift_right_logical x y_shift)
      in
      (input, expected));
  };
  {
    testname = "strlit";
    filename = "cases/strlit.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "wow a string literal!!!!\n"));
  };
]

(* 
  ==== Monomorphism Tests ====
  These test the monomorphism pass of the compiler. The test suite is not designed
  to test this well (very focused on the io) but still they provide a certain level of confidence
  since these examples would not work without the monomorphism pass. Also note that these tests are more
  meaningfull for the compiler than for the interpreter since the interpreter is not really in need
  of the monomorphism pass. 
*)

let monomorphism_tests = [
  {
    testname = "cyclepolymorph";
    filename = "cases/cyclepolymorph.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "hello\n5\n"));
  };
  {
    testname = "partialpoly";
    filename = "cases/partialpoly.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "6\n"));
  };
  {
    testname = "tuppartialpoly";
    filename = "cases/tuppartialpoly.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "44\n"));
  };
  {
    testname = "v4parsertest";
    filename = "cases/v4parsertest.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "9\n"));
  };
]


(*
  ==== Legacy Tests ====
  These tests are the legacy tests the I kinda liked so they can stay
*)
let legacy_tests = [
  {
    testname = "churchbool";
    filename = "cases/churchbool.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "100\n"));
  };
  {
    testname = "curry";
    filename = "cases/curry.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "15\n"));
  };
  {
    testname = "funclist";
    filename = "cases/funclist.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "2\n"));
  }; 
  {
    testname = "pipe";
    filename = "cases/pipe.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "11\n"));
  }; 
  {
    testname = "quad";
    filename = "cases/quad.intlang";
    iterations = 1;
    generator = (fun _ -> ("", "4\n"));
  }
]
  


(*
  ==== Lib Tests ====
  These tests the basics of intlangstdlib: math, vector, queue, sort and search.
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

let cmp_int32 a b =
  if Int32.compare a b < 0 then -1l else if Int32.compare a b > 0 then 1l else 0l

let inbounds_int32 low high value =
  if Int32.compare low value <= 0 && Int32.compare value high < 0 then 1l else 0l

let cmp_int32_lists lst0 lst1 =
  let elcmp = (fun x y -> Int32.to_int @@ cmp_int32 x y) in
  if List.compare elcmp lst0 lst1 < 0 then -1l else if List.compare elcmp lst0 lst1 > 0 then 1l else 0l

let search_int32lst (lst : int32 list) (target : int32) =
  List.fold_right (fun (idx, x) acc -> if Int32.compare x target = 0 then idx else acc) (List.mapi (fun idx x -> (Int32.of_int idx, x)) lst) (-1l) 

let lib_tests = [
  {
    testname = "math_libtest";
    filename = "cases/math_libtest.intlang";
    iterations = 32;
    generator = (fun i ->
      let sqrt_input = rand_int32_ranged 2026070701l i 0l 10000l in
      let pow_base = rand_int32_ranged 2026070702l i (-100l) 100l in
      let pow_exp = rand_int32_ranged 2026070703l i 1l 4l in
      let gcd_a = rand_int32_ranged 2026070704l i 0l 100000l in
      let gcd_b = rand_int32_ranged 2026070705l i 0l 100000l in
      let cmp_a = rand_int32 2026070706l i in
      let cmp_b = rand_int32 2026070707l i in
      let low = rand_int32 2026070708l i in
      let value = rand_int32 2026070709l i in
      let high = rand_int32 2026070710l i in
      let input = prln_int32lst [sqrt_input; pow_base; pow_exp; gcd_a; gcd_b; cmp_a; cmp_b; low; high; value] in
      let expected = String.concat "" [
        prln_int32 (isqrt_i32 sqrt_input);
        prln_int32 (pow_i32 pow_base pow_exp);
        prln_int32 (gcd_i32 gcd_a gcd_b);
        prln_int32 (cmp_int32 cmp_a cmp_b);
        prln_int32 (inbounds_int32 low high value)
      ] in
      (input, expected));
  };
  {
    testname = "vector_libtest_1d_i32";
    filename = "cases/vector_libtest_1d_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let vi32 = rand_int32lst_ranged 2026070711l i (-10000l) 10000l 32l in
      let double = List.map (Int32.mul 2l) vi32 in
      let reverse = List.rev vi32 in
      let quotient = List.map2 (fun a b -> Int32.div a (if b = 0l then 1l else b)) vi32 reverse in
      let copied = vi32 @ vi32 in
      let addreduce_left = List.fold_left Int32.add 0l vi32 in
      let addreduce_quotient = List.fold_left2 (fun acc a b -> Int32.add acc (Int32.div b (if a = 0l then 1l else a))) 0l vi32 reverse in
      let addreduce_right = List.fold_right Int32.add vi32 0l in
      let middle = (List.length vi32) / 2 in
      let vi32larger = List.mapi (fun idx value -> if idx = middle then Int32.add value 1l else value) vi32 in
      let input = prln_int32lst vi32 in
      let expected = String.concat "" [
        prln_int32lst double;
        prln_int32lst reverse;
        prln_int32lst quotient;
        prln_int32lst copied;
        prln_int32 addreduce_left;
        prln_int32 addreduce_quotient;
        prln_int32 addreduce_right;
        prln_int32 (cmp_int32_lists vi32 vi32);
        prln_int32 (cmp_int32_lists vi32 vi32larger);
        prln_int32 (cmp_int32_lists vi32larger vi32);
        prln_int32 (cmp_int32_lists vi32 copied);
        prln_int32 (cmp_int32_lists copied vi32);
        prln_int32lst_trailing vi32
      ] in
      (input, expected));
  };
  {
    testname = "queue_libtest_i32";
    filename = "cases/queue_libtest_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let seed0 = rand_int32 444919671l i in
      let rounds = rand_int32_ranged seed0 0 1l 10l in
      let input_ref = ref (prln_int32 rounds) in
      let expected_ref = ref "" in
      let queue = Queue.create () in
      for round = 0 to Int32.to_int rounds - 1 do
        let seed1 = rand_int32 903114354l round in
        let enqueuelst = rand_int32lst_ranged seed1 0 (-1000l) (1000l) (Int32.of_int (round*20 + 200)) in
        let dequeue_count = rand_int32_ranged seed1 1 0l (Int32.of_int (round*20 + 200)) in
        input_ref := !input_ref ^ prln_int32lst enqueuelst ^ prln_int32 dequeue_count;
        List.iter (fun x -> Queue.push x queue) enqueuelst;
        expected_ref := !expected_ref ^ (Queue.length queue |> Int32.of_int |> prln_int32);
        for _ = 0 to Int32.to_int dequeue_count - 1 do
          expected_ref := !expected_ref ^ (if Queue.is_empty queue then "" else (Int32.to_string (Queue.pop queue)) ^ ",");
        done;
        expected_ref := !expected_ref ^ "\n";
      done;
      (!input_ref, !expected_ref))
  };
  {
    testname = "sort_libtest_i32";
    filename = "cases/sort_libtest_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let input_vec = rand_int32lst_ranged 30659444l i (-30l) 30l 64l in
      let sorted_vec = List.sort Int32.compare input_vec in
      let input = prln_int32lst input_vec in
      let expected = prln_int32lst sorted_vec in
      (input, expected));
  };
  {
    testname = "search_libtest_i32";
    filename = "cases/search_libtest_i32.intlang";
    iterations = 32;
    generator = (fun i ->
      let input_vec0 = rand_int32lst_ranged 30659444l i (-30l) 30l 32l in
      let target = rand_int32_ranged 80773817l i (-30l) (30l) in
      let input_vec1 = rand_int32lst_ranged 19772196l i (-30l) 30l 32l in
      let input_vec = input_vec0 @ [target] @ input_vec1 in
      let sorted_vec = List.sort Int32.compare input_vec in
      let input = prln_int32 target ^
                  prln_int32lst input_vec ^ 
                  prln_int32lst sorted_vec in
      let expected =  prln_int32 (search_int32lst input_vec target) ^
                      prln_int32 (search_int32lst sorted_vec target) in
      (input, expected))
  }
]

(*
  ==== Mat Tests ====
  These tests test mat.intlang (matrix library)
*)

let matmul_flat rows shared cols lhs rhs =
  let rows = Int32.to_int rows in
  let shared = Int32.to_int shared in
  let cols = Int32.to_int cols in
  Array.init (rows * cols) (fun idx ->
    let row = idx / cols in
    let col = idx mod cols in
    let acc = ref 0l in
    for k = 0 to shared - 1 do
      let lhs_idx = row * shared + k in
      let rhs_idx = k * cols + col in
      acc := Int32.add !acc (Int32.mul lhs.(lhs_idx) rhs.(rhs_idx))
    done;
    !acc)

let matadd_flat lhs rhs =
  Array.init (Array.length lhs) (fun idx -> Int32.add lhs.(idx) rhs.(idx))

let matsmul_flat lhs scalar =
  Array.init (Array.length lhs) (fun idx -> Int32.mul lhs.(idx) scalar)

let matsub_flat lhs rhs =
  Array.init (Array.length lhs) (fun idx -> Int32.sub lhs.(idx) rhs.(idx))

let matid_flat n =
  let n = Int32.to_int n in
  Array.init (n * n) (fun idx -> if idx / n = idx mod n then 1l else 0l)

let matsqtrans_flat n lhs =
  let n = Int32.to_int n in
  Array.init (n * n) (fun idx ->
    let row = idx / n in
    let col = idx mod n in
    lhs.(col * n + row))

let mat_tests = [
    {
    testname = "mat_libtest_nonsq";
    filename = "cases/mat_libtest_nonsq.intlang";
    iterations = 50;
    generator = (fun i ->
      let seed0 = rand_int32 820401106l i in
      let rows = rand_int32_ranged seed0 0 1l 15l in
      let shared = rand_int32_ranged seed0 1 1l 15l in
      let cols = rand_int32_ranged seed0 2 1l 15l in
      let a = rand_int32array_ranged seed0 3 (-10000l) 10000l (Int32.mul rows shared) in
      let a' = rand_int32array_ranged seed0 4 (-10000l) 10000l (Int32.mul rows shared) in
      let b = rand_int32array_ranged seed0 5 (-10000l) 10000l (Int32.mul shared cols) in
      let ab = matmul_flat rows shared cols a b in
      let a3 = matsmul_flat a 3l in
      let apa' = matadd_flat a a' in
      let a2 = matsub_flat a3 a in
      let input = String.concat "" [
        prln_int32 rows;
        prln_int32 shared;
        prln_int32 cols;
        prln_int32array a;
        prln_int32array a';
        prln_int32array b
      ] in
      let expected = String.concat "" [
        prln_int32array ab;
        prln_int32array a3;
        prln_int32array apa';
        prln_int32array a2
      ] in
      (input, expected));
    };
    {
    testname = "mat_libtest_sq";
    filename = "cases/mat_libtest_sq.intlang";
    iterations = 50;
    generator = (fun i ->
      let seed0 = rand_int32 97757234l i in
      let dim = rand_int32_ranged seed0 0 1l 15l in
      let a = rand_int32array_ranged seed0 1 (-10000l) 10000l (Int32.mul dim dim) in
      let aT = matsqtrans_flat dim a in
      let idmat = matid_flat dim in
      let input = String.concat "" [
        prln_int32 dim;
        prln_int32array a
      ] in
      let expected = String.concat "" [
        prln_int32array aT;
        prln_int32array idmat
      ] in
      (input, expected));
    };
]


(*
  ==== Graph Tests ====
  These tests test the graph.inlang library
*)

module Int32Vertex = struct
  type t = int32
  let compare = Int32.compare
  let equal = Int32.equal
  let hash = Hashtbl.hash
end

module G = Graph.Persistent.Digraph.Concrete(Int32Vertex)

(* Returns: (discover_array, finish_array, parent_array) all containing int32 *)
let dfs_canonicalord_ocamlvers g size start =
  if G.is_empty g then ([||], [||], [||])
  else
    (*1. Make sure start vertex exists*)
    let g = G.add_vertex g start in 

    (* 2. Initialize arrays with int32 fallback values (-1l) *)
    let discover_arr = Array.make size (-1l) in
    let finish_arr = Array.make size (-1l) in
    let parent_arr = Array.make size (-1l) in
    let time = ref 0l in

    (* 3. Core DFS tracking with int32 time updates *)
    let rec dfs_visit u =
      let u_idx = Int32.to_int u in
      discover_arr.(u_idx) <- !time;
      time := Int32.add !time 1l;
      
      G.iter_succ (fun v ->
        let v_idx = Int32.to_int v in
        (* If discover time is still -1l, it hasn't been visited *)
        if discover_arr.(v_idx) = -1l then begin
          parent_arr.(v_idx) <- u;
          dfs_visit v
        end
      ) g u;
      
      finish_arr.(u_idx) <- !time;
      time := Int32.add !time 1l
    in

    (* 4. Execute single-shot traversal from the start node *)
    parent_arr.(Int32.to_int start) <- -1l;
    dfs_visit start;
    (discover_arr, finish_arr, parent_arr)


let graph_tests = [
  {
    testname = "graph_libtest";
    filename = "cases/graph_libtest.intlang";
    iterations = 64;
    generator = (fun i ->
      let seed0 = rand_int32 2026070712l (i+0) in
      let n = rand_int32_ranged seed0 0 1l 100l in
      let nm1 = Int32.sub n 1l in
      let edges_toadd = rand_int32tuplst_ranged seed0 1 0l nm1 500l in
      let checks0 = rand_int32tuplst_ranged seed0 2 0l nm1 200l in
      let edges_toremove = rand_int32tuplst_ranged seed0 3 0l nm1 100l in
      let checks1 = rand_int32tuplst_ranged seed0 4 0l nm1 200l in
      let g = G.empty in
      let g = List.fold_left (fun acc (f, t) -> G.add_edge acc f t) g edges_toadd in
      let checks0_results = List.map (fun (f, t) -> if G.mem_edge g f t then 1l else 0l) checks0 in
      let g = List.fold_left (fun acc (f, t) -> try G.remove_edge acc f t with Invalid_argument _ -> acc) g edges_toremove in
      let checks1_results = List.map (fun (f, t) -> if G.mem_edge g f t then 1l else 0l) checks1 in
      let start = rand_int32_ranged seed0 5 0l (Int32.sub n 1l) in
      let (discover, finish, parent) = dfs_canonicalord_ocamlvers g (Int32.to_int n) start in
      let input = prln_int32 n ^
                  prln_int32tuplst edges_toadd ^
                  prln_int32tuplst checks0 ^
                  prln_int32tuplst edges_toremove ^
                  prln_int32tuplst checks1 ^
                  prln_int32 start in
      let expected = prln_int32lst checks0_results ^
                     prln_int32lst checks1_results ^
                     prln_int32array discover ^
                     prln_int32array finish ^
                     prln_int32array parent in

      (input, expected));
  };
]



let tests = [
  ("IOBasic", iobasic_tests);
  ("LangBasic", langbasic_tests);
  ("IOLib", iolib_tests);
  ("Lang", lang_tests);
  ("Lib", lib_tests);
  ("Mat", mat_tests);
  ("Graph", graph_tests);
]