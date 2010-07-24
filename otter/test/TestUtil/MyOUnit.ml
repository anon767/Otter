(* include the test type *)
type test = OUnit.test = TestCase of (unit -> unit) | TestList of test list | TestLabel of string * test

exception MyOUnitFailure

(* error message buffer *)
let buffer = Buffer.create 4096
let formatter = Format.formatter_of_buffer buffer
let assert_log format = Format.fprintf formatter format


(* test wrapper that sets up the log and reports unexpected exceptions *)
let wrap_test testfn = fun () ->
    (* enable backtrace *)
    let prev_backtrace = Printexc.backtrace_status () in
    Printexc.record_backtrace true;
    (* reset printing boxes *)
    Format.pp_print_flush formatter ();
    Buffer.clear buffer;
    (* indent *)
    assert_log "@\n  @[";
    (* run test *)
    try testfn () with e -> begin
        begin match e with
            | MyOUnitFailure -> ()
            | _ ->
                (* report unexpected exceptions *)
                let b = Str.split (Str.regexp "\n") (Printexc.get_backtrace ()) in
                let line_printer ff l =
                    ignore (List.fold_left (fun b e -> Format.fprintf ff "%(%)%s" b e; "@\n") "" l)
                in
                assert_log "@[<2>Unexpected exception: %s@\n@[<v>%a@]@]" (Printexc.to_string e) line_printer b
        end;
        Printexc.record_backtrace prev_backtrace;
        Format.pp_print_flush formatter ();
        ignore (OUnit.assert_failure (Buffer.contents buffer))
    end;
    Printexc.record_backtrace prev_backtrace


(* test wrapper that forks the process before running the test *)
let fork_test testfn = fun () ->
    (* first, flush stdout to avoid printing twice *)
    Format.print_flush ();
    (* create a pipe to proxy the error report from child to parent *)
    let fdin, fdout = Unix.pipe () in
    let child = Unix.fork () in
    if child = 0 then begin
        (* child process runs the test and proxies the result to the parent *)
        Unix.close fdin;
        begin try wrap_test testfn () with
            | Failure s ->
                ignore (Unix.single_write fdout s 0 (String.length s));
                exit 1
        end;
        exit 0
    end else begin
        (* parent process waits for child, and captures the result *)
        Unix.close fdout;
        let message = Buffer.create 4096 in
        let tmp = String.create 4096 in
        while match Unix.read fdin tmp 0 4096 with
            | 0 -> false
            | n -> Buffer.add_substring message tmp 0 n; true
        do () done;
        Unix.close fdin;
        (* get the child's exit status *)
        let _, status = Unix.waitpid [] child in
        match status with
            | Unix.WEXITED 0 ->
                ()
            | Unix.WEXITED _ ->
                OUnit.assert_failure (Buffer.contents message)
            | Unix.WSIGNALED i ->
                OUnit.assert_failure (Format.sprintf "Test process unexpectedly killed by signal %d." i)
            | Unix.WSTOPPED i ->
                (* this should never occur since waitpid wasn't given the WUNTRACED flag *)
                OUnit.assert_failure (Format.sprintf "Test process unexpectedly stopped by signal %d." i)
    end


(* test wrapper that creates a temporary file, fills it some content, and passes it to the test *)
let test_string_as_file base ext content test =
    OUnit.bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file base ext in
        output_string fileout content;
        close_out fileout;
        filename
    end test Unix.unlink


(** Convenience test helper for testing all files in a directory recursively.
        @param path is the path to the directory in which find files
        @param test is the test to apply to each file found, given as path relative from the original directory
        @return a [TestList] of tests applied to files found
 *)
let test_dir path test =
    let rec test_dir relpath dir tests =
        (* first, sort down *)
        Array.sort (fun x y -> -(String.compare x y)) dir;
        (* then, iterate left-to-right, so output list will become sorted up *)
        Array.fold_left begin fun tests filename ->
            if filename.[0] = '.' then (* skip hidden files *)
                tests
            else begin
                let relpath = Filename.concat relpath filename in
                let fullpath = Filename.concat path relpath in
                if Sys.is_directory fullpath then
                    test_dir relpath (Sys.readdir fullpath) tests (* recurse into directories *)
                else
                    (test relpath)::tests (* add files *)
            end
        end tests dir
    in
    TestList (test_dir "" (Sys.readdir path) [])


(* redefine OUnit functions to use the above buffer, test wrapper, and Format-based printer *)
let (>:) = OUnit.(>:)
let (>::) label testfn = OUnit.(>::) label (fork_test testfn)
let (>:::) = OUnit.(>:::)
let bracket = OUnit.bracket
let run_test_tt_main = OUnit.run_test_tt_main

let assert_failure format =
    Format.kfprintf (fun _ -> raise MyOUnitFailure) formatter format

let assert_bool msg flag =
    if not flag then assert_failure "@[%s@]@." msg

let assert_string msg =
    if not (msg = "") then assert_failure "@[%s@]@." msg

let assert_equal ?(cmp=Pervasives.(=)) ?printer ?(msg="") expected actual =
    if not (cmp expected actual) then begin match printer with
        | Some p -> assert_failure "@[%s@]@\n  @[@[<2>expected:@ %a@]@ @[<2>but got:@ %a@]@]" msg p expected p actual
        | None -> assert_failure "@[%s@]@\n  not equal" msg
    end


(* assert that a result matches a pattern (given as a function with a refutable pattern) *)
let assert_match ?printer expected_match actual =
    try expected_match actual with Match_failure _ -> begin match printer with
        | Some p -> assert_failure "@[<2>Did not match:@ %a@]" p actual
        | None -> assert_failure "Match failure"
    end


(* assert that collection contains elements in list, given a membership function mem *)
let assert_mem ?printer mem list collection =
    let bad = List.filter (fun el -> not (mem el collection)) list in
    if bad != [] then begin match printer with
        | Some p -> assert_failure "@[<v2>Not in:@ %a@]" (fun ff -> List.iter (Format.fprintf ff "@[%a@]@ " p)) bad
        | None -> assert_failure "Not in collection"
    end


(*
 * Convenience functions comparing as well as printing option and list types
 *)

let option_equal eq x y = match x, y with
    | Some x, Some y -> eq x y
    | None, None -> true
    | _ -> false

let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"

let rec list_equal eq x y = match x, y with
    | x::xs, y::ys when eq x y -> list_equal eq xs ys
    | [], [] -> true
    | _ -> false

let list_printer printer sep ff list =
    ignore (List.fold_left (fun sep x -> Format.fprintf ff "%(%)@[%a@]" sep printer x; sep) "" list)

