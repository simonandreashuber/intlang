open Tests

exception TestExecutionError of string

(* Helper to read all contents of an in_channel until EOF *)
let read_all_channel ic =
  let buf = Buffer.create 1024 in
  let chunk = Bytes.create 1024 in
  let rec loop () =
    let bytes_read = input ic chunk 0 1024 in
    if bytes_read > 0 then begin
      Buffer.add_subbytes buf chunk 0 bytes_read;
      loop ()
    end
  in
  try 
    loop (); 
    Buffer.contents buf
  with End_of_file -> 
    Buffer.contents buf

(* Generates a clean, line-by-line diff pointing to the first mismatch *)
let generate_diff expected actual =
  let exp_lines = String.split_on_char '\n' expected in
  let act_lines = String.split_on_char '\n' actual in
  
  let rec find_mismatch line_num l1 l2 =
    match l1, l2 with
    | [], [] -> None
    | h1::t1, h2::t2 when h1 = h2 -> find_mismatch (line_num + 1) t1 t2
    | h1::_, h2::_ -> 
        Some (Printf.sprintf "Mismatch at line %d:\n  - EXPECTED: %S\n  + ACTUAL  : %S" line_num h1 h2)
    | [], h2::_ -> 
        Some (Printf.sprintf "Mismatch at line %d:\n  - EXPECTED: <End of Output>\n  + ACTUAL  : %S\n    (Interpreter emitted extra lines)" line_num h2)
    | h1::_, [] -> 
        Some (Printf.sprintf "Mismatch at line %d:\n  - EXPECTED: %S\n  + ACTUAL  : <End of Output>\n    (Interpreter stopped early)" line_num h1)
  in
  match find_mismatch 1 exp_lines act_lines with
  | Some diff -> diff
  | None -> "Outputs differ, but line-by-line match failed to find exact point (possible line ending mismatch)."

(* Runs a single process, feeds it input, and returns the output/error *)
let execute_process cmd input_data =
  let (stdout_ch, stdin_ch, stderr_ch) = Unix.open_process_full cmd (Unix.environment ()) in
  
  (* Thread-safe references to capture string data *)
  let stdout_output = ref "" in
  let stderr_output = ref "" in
  
  (* Spawn concurrent threads to drain stdout and stderr parallelly *)
  let stdout_thread = Thread.create (fun () -> stdout_output := read_all_channel stdout_ch) () in
  let stderr_thread = Thread.create (fun () -> stderr_output := read_all_channel stderr_ch) () in
  
  try
    (* Main thread pushes data into stdin *)
    output_string stdin_ch input_data;
    close_out stdin_ch; (* Signal EOF to the interpreter *)

    (* Wait for the background reading threads to fully consume remaining buffers *)
    Thread.join stdout_thread;
    Thread.join stderr_thread;

    let status = Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch) in
    match status with
    | Unix.WEXITED 0 -> Ok !stdout_output
    | Unix.WEXITED code -> 
        Error (Printf.sprintf "Non-zero exit code %d.\nStderr:\n%s\nInput:\n%s" code !stderr_output input_data)
    | _ -> Error "Process terminated abnormally."
  with e ->
    (* Clean up resources if an unexpected exception occurs *)
    ignore (Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch));
    Error (Printf.sprintf "Exception during execution: %s" (Printexc.to_string e))

(* BATCH MODE: Generates all data, runs once with --test N *)
let run_batch interp_binary case =
  let input_buf = Buffer.create 4096 in
  let expect_buf = Buffer.create 4096 in
  
  for i = 0 to case.iterations - 1 do
    let (in_str, exp_str) = case.generator i in
    Buffer.add_string input_buf in_str;
    Buffer.add_string expect_buf exp_str
  done;

  let all_input = Buffer.contents input_buf in
  let all_expected = Buffer.contents expect_buf in
  let cmd = Printf.sprintf "%s --stdlibpath %s --testmir %d %s" 
      (Filename.quote interp_binary) 
      (Filename.quote (Sys.getcwd () ^ "/intlangstdlib/")) 
      case.iterations 
      (Filename.quote case.filename) in

  match execute_process cmd all_input with
  | Ok actual ->
      if actual = all_expected then None
      else Some (Printf.sprintf "Batch mismatch:\n%s\n" (generate_diff all_expected actual))
  | Error msg -> Some (Printf.sprintf "Batch Execution Failed:\n%s" msg)

(* SEPARATE MODE: Runs a new process for every iteration *)
let run_separate interp_binary case =
  let cmd = Printf.sprintf "%s --stdlibpath %s --testmir 1 %s" 
      (Filename.quote interp_binary) 
      (Filename.quote (Sys.getcwd () ^ "/intlangstdlib/")) 
      (Filename.quote case.filename) in
  
  let rec loop i =
    if i >= case.iterations then None
    else
      let (in_str, exp_str) = case.generator i in
      match execute_process cmd in_str with
      | Ok actual ->
          if actual = exp_str then loop (i + 1)
          else Some (Printf.sprintf "Iteration %d mismatch:\n%s\nInput:\n%s\nOutput:\n%s\nExpected:\n%s" i (generate_diff exp_str actual) in_str actual exp_str)
      | Error msg -> Some (Printf.sprintf "Iteration %d Execution Failed:\n%s" i msg)
  in
  loop 0

let () =
  (* CLI Parsing *)
  let separate_mode = ref false in
  let interp_binary = ref "" in

  let speclist = [
    ("--separate", Arg.Set separate_mode, "Run each test iteration in a separate process");
  ] in
  let usage_msg = "Usage: test_runner [--separate] <interp_binary>" in
  
  Arg.parse speclist (fun s -> interp_binary := s) usage_msg;

  if !interp_binary = "" then begin
    Printf.eprintf "Error: Target interpreter binary path argument missing.\n%s\n" usage_msg;
    exit 1
  end;

  let run_test = if !separate_mode then run_separate else run_batch in
  let global_failed = ref false in

  Printf.printf "=== Starting Tests (Mode: %s) ===\n" (if !separate_mode then "Separate" else "Batch");

  List.iter (fun (testgroupname, testcases) ->
    (* Evaluate all cases in the group first *)
    let results = List.map (fun case -> (case, run_test !interp_binary case)) testcases in
    
    let group_failed = List.exists (fun (_, res) -> res <> None) results in

    if not group_failed then begin
      (* Print single line if everything passes *)
      Printf.printf "[TEST GROUP PASSED]: %s (All %d cases)\n" testgroupname (List.length testcases);
      flush stdout
    end else begin
      (* Print detailed lines if something failed *)
      global_failed := true;
      Printf.printf "\n=== TEST GROUP FAILED: %s ===\n" testgroupname;
      
      List.iter (fun (case, res) ->
        match res with
        | None -> Printf.printf "  [PASS] %s\n" case.testname
        | Some err -> 
            Printf.printf "  [FAIL] %s\n" case.testname;
            Printf.printf "         %s\n" (String.concat "\n         " (String.split_on_char '\n' err))
      ) results;
      flush stdout;
    end
  ) tests;

  if !global_failed then exit 1 else exit 0