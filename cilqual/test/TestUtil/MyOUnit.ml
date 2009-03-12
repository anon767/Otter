open OUnit

exception MyOUnitFailure

(* error message buffer *)
let buffer = Buffer.create 4096
let formatter = Format.formatter_of_buffer buffer
let assert_logf format = Format.fprintf formatter format

(* test wrapper that sets up the log and reports unexpected exceptions *)
let (>::) label testfn =
    OUnit.(>::) label begin fun () ->
        (* enable backtrace *)
        let prev_backtrace = Printexc.backtrace_status () in
        Printexc.record_backtrace true;
        (* reset printing boxes *)
        Format.pp_print_flush formatter ();
        Buffer.clear buffer;
        (* indent *)
        assert_logf "\n  @[";
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
                    assert_logf "@[<2>Unexpected exception: %s@\n@[<v>%a@]@]" (Printexc.to_string e) line_printer b
            end;
            Printexc.record_backtrace prev_backtrace;
            Format.pp_print_flush formatter ();
            ignore (OUnit.assert_failure (Buffer.contents buffer));
        end;
        Printexc.record_backtrace prev_backtrace
    end

(* redefine OUnit functions to use the above buffer, test wrapper, and Format-based printer *)
let (>:::) = OUnit.(>:::)
let bracket = OUnit.bracket

let assert_failure () = raise MyOUnitFailure

let assert_bool msg flag =
    if not flag then begin
        assert_logf "@[%s@]@." msg;
        assert_failure ()
    end

let assert_equal ?(cmp=Pervasives.(=)) ?printer ?(msg="") expected actual =
    if not (cmp expected actual) then begin
        begin match printer with
            | Some p -> assert_logf "@[%s@]@\n  @[@[<2>expected:@ %a@]@ @[<2>but got:@ %a@]@]" msg p expected p actual
            | None -> assert_logf "@[%s@]@\n  not equal" msg
        end;
        assert_failure ()
    end


(* assert that a result matches a pattern (given as a function with a refutable pattern) *)
let assert_match ?printer expected_match actual =
    try expected_match actual with Match_failure _ -> begin
        begin match printer with
            | Some p -> assert_logf "@[<2>Did not match:@ %a@]" p actual;
            | None -> assert_logf "Match failure"
        end;
        assert_failure ()
    end


(* assert that collection contains elements in list, given a membership function mem *)
let assert_mem ?printer mem list collection =
    let bad = List.filter (fun el -> not (mem el collection)) list in
    if bad != [] then begin
        begin match printer with
            | Some p -> assert_logf "@[<v2>Not in:@ %a@]" (fun ff -> List.iter (Format.fprintf ff "@[%a@]@ " p)) bad
            | None -> assert_logf "Not in collection"
        end;
        assert_failure ()
    end

