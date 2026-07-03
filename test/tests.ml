type test_case = {
  testname   : string;
  filename   : string;
  iterations : int;
  generator  : int -> (string * string);
}

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
  ("strlib", strlib_tests)
]