open Tests

let is_stdout_nonempty (stdout_ch : in_channel) : bool =
  let fd = Unix.descr_of_in_channel stdout_ch in
  let (ready, _, _) = Unix.select [fd] [] [] 0.0 in
  ready <> []

let run_test interp_binary case =

  (*
    General Methodology:
      - The main idea is to have some .intlang file that has a main and does IO, for each such file we also have one or more entries in our 
        tests list (in tests.ml) that specifies an integer range and a function mapping this integer range to input output string pairs
      - the main executable has a special flag where one execution actually runs the interpreter many times
        - after each interpreter iteration the main proc will then print REPDONE to stderr which the test runner can capture and know 
          that the next test iteration can proceed
        - this way one proc is needed per "test" not per "test iteration"
  *)

  (*using open_process_full allows full control over stdin and stdout*)
  let cmd = Printf.sprintf "%s --stdlibpath %s --test %d %s" interp_binary ((Sys.getcwd ()) ^ "/intlangstdlib/") case.iterations case.filename in
  let (stdout_ch, stdin_ch, stderr_ch) = Unix.open_process_full cmd [||] in

  try
    for i = 0 to case.iterations - 1 do
      let (input_data, expected_output) = case.generator i in
      let expected_len = String.length expected_output in

      output_string stdin_ch input_data;
      flush stdin_ch;

      let actual_output = really_input_string stdout_ch expected_len in
      let sync_token = input_line stderr_ch in
      
      if sync_token <> "REPDONE" then begin
        let leftover_error = try input_line stderr_ch with End_of_file -> "" in
        Printf.eprintf "\n[TEST FAILED]: %s (Iteration %d)\n" case.filename i;
        Printf.eprintf "Reason: Got someting in stderr that was not REPDONE\n";
        Printf.eprintf "=== INTERPRETER STDERR ===\n%s\n%s\n" sync_token leftover_error;
        exit 1
      end;

      if is_stdout_nonempty stdout_ch then begin
        let fd = Unix.descr_of_in_channel stdout_ch in
        let buf = Bytes.create 100 in
        let bytes_read = Unix.read fd buf 0 100 in
        let extra_garbage = Bytes.sub_string buf 0 bytes_read in

        Printf.eprintf "\n[TEST FAILED]: %s (Iteration %d)\n" case.testname i;
        Printf.eprintf "Reason: Interpreter emitted MORE characters than specified!\n";
        Printf.eprintf "=== EXPECTED ===\n%S\n=== ACTUAL ===\n%S\n=== EXTRA ===\n%S\n" 
          expected_output actual_output extra_garbage;
        exit 1
      end;

      if actual_output <> expected_output then begin
        Printf.eprintf "\n[TEST FAILED]: %s (Iteration %d)\n" case.testname i;
        Printf.eprintf "=== EXPECTED ===\n%S\n=== ACTUAL ===\n%S\n\n" expected_output actual_output;
        exit 1
      end
    done;

    close_out stdin_ch;
    let status = Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch) in
    
    match status with
    | Unix.WEXITED 0 -> 
        Printf.printf "[TEST PASSED]: %s Passed all %d iterations successfully.\n" case.testname case.iterations
    | Unix.WEXITED code -> 
        Printf.eprintf "\n[TEST FAILED]: %s\nReason: Non-zero exit code %d\n" case.testname code;
        exit 1
    | _ -> 
        exit 1

  with
  | End_of_file ->
      let rec gather_stderr acc =
        try gather_stderr (input_line stderr_ch :: acc) with End_of_file -> String.concat "\n" (List.rev acc)
      in
      let stderr_log = gather_stderr [] in
      ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));

      Printf.eprintf "\n[TEST FAILED]: %s\nReason: Interpreter pipe severed cleanly (End_of_file).\n" case.testname;
      if stderr_log <> "" then Printf.eprintf "=== INTERPRETER STDERR ===\n%s\n" stderr_log;
      exit 1
  | e ->
      ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));
      raise e

let () = 
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Error: Test runner missing target interpreter binary path argument.\n";
    exit 1
  end;
  let interp_binary = Sys.argv.(1) in

  Printf.printf "=== Starting Tests ===\n";
  List.iter (run_test interp_binary) tests