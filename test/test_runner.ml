open Tests

exception TestExecutionError of string


let is_stdout_nonempty (stdout_ch : in_channel) : bool =
  let fd = Unix.descr_of_in_channel stdout_ch in
  let (ready, _, _) = Unix.select [fd] [] [] 0.0 in
  ready <> []

(* Returns Some error_msg string if the test failed, or None if it passed *)
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
        let msg = Printf.sprintf "\n[TEST FAILED]: %s (Iteration %d)\nReason: Got someting in stderr that was not REPDONE\n=== INTERPRETER STDERR ===\n%s\n%s\n" case.filename i sync_token leftover_error in
        raise (TestExecutionError msg)
      end;

      if is_stdout_nonempty stdout_ch then begin
        let fd = Unix.descr_of_in_channel stdout_ch in
        let buf = Bytes.create 100 in
        let bytes_read = Unix.read fd buf 0 100 in
        let extra_garbage = Bytes.sub_string buf 0 bytes_read in
        let msg = Printf.sprintf "\n[TEST FAILED]: %s (Iteration %d)\nReason: Interpreter emitted MORE characters than specified!\n=== EXPECTED ===\n%S\n=== ACTUAL ===\n%S\n=== EXTRA ===\n%S\n" 
          case.testname i expected_output actual_output extra_garbage in
        raise (TestExecutionError msg)
      end;

      if actual_output <> expected_output then begin
        let msg = Printf.sprintf "\n[TEST FAILED]: %s (Iteration %d)\n=== EXPECTED ===\n%S\n=== ACTUAL ===\n%S\n\n" case.testname i expected_output actual_output in
        raise (TestExecutionError msg)
      end
    done;

    close_out stdin_ch;
    let status = Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch) in
    
    match status with
    | Unix.WEXITED 0 -> None
    | Unix.WEXITED code -> 
        let msg = Printf.sprintf "\n[TEST FAILED]: %s\nReason: Non-zero exit code %d\n" case.testname code in
        Some msg
    | _ -> Some (Printf.sprintf "\n[TEST FAILED]: %s\nReason: Process terminated abnormally\n" case.testname)

  with
  | TestExecutionError msg ->
      ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));
      Some msg
  | End_of_file ->
      let rec gather_stderr acc =
        try gather_stderr (input_line stderr_ch :: acc) with End_of_file -> String.concat "\n" (List.rev acc)
      in
      let stderr_log = gather_stderr [] in
      ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));
      let msg = Printf.sprintf "\n[TEST FAILED]: %s\nReason: Interpreter pipe severed cleanly (End_of_file).\n" case.testname ^
                (if stderr_log <> "" then Printf.sprintf "=== INTERPRETER STDERR ===\n%s\n" stderr_log else "") in
      Some msg
  | e ->
      ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));
      Some (Printf.sprintf "\n[TEST FAILED]: %s\nReason: Unexpected exception: %s\n" case.testname (Printexc.to_string e))

let () = 
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Error: Test runner missing target interpreter binary path argument.\n";
    exit 1
  end;
  let interp_binary = Sys.argv.(1) in

  Printf.printf "=== Starting Tests ===\n";
  let global_failed = ref false in

  List.iter (fun (testgroupname, testcases) ->
    let total_tests = List.length testcases in
    let group_failures = ref [] in

    List.iter (fun case ->
      match run_test interp_binary case with
      | None -> ()
      | Some error_msg -> group_failures := error_msg :: !group_failures
    ) testcases;

    if !group_failures = [] then
      (Printf.printf "[TEST GROUP PASSED]: %s (All %d tests passed successfully.)\n" testgroupname total_tests; flush stdout)
    else begin
      global_failed := true;
      Printf.printf "=== Running Test Group: %s ===\n" testgroupname;
      List.iter (fun msg -> Printf.printf "%s" msg) (List.rev !group_failures);
      flush stderr;
    end
  ) tests;

  if !global_failed then exit 1 else exit 0