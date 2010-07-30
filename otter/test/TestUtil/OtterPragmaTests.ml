(** Testing framework for Otter where tests can be written as C files, and test expectations are specified using
    [#pragma] directives in the test files themselves.

    Test expectations are given as [#pragma] directives in test files. Except for [#pragma command_line(...)] and
    [#pragma has_failing_assertions], the directives will be interpreted in the order they are written.

    The following [#pragma] directives are understood:
        - [#pragma command_line(<string argument>, ...)] specifies the command line arguments to be passed to
            [main()]. E.g., [#pragma command_line("foo", "bar")].
        - [#pragma has_failing_assertions] specifies that failing assertions should be expected. Conversely, {e not
            providing} this directive specifies that failing assertions should not be expected.
        - [#pragma expect_return(<assertion expression>, ...)] specifies that there should be a {!Types.Return}
            in which a list of assertions hold. The first matching result will be removed from further processing.
            E.g., [#pragma expect_return(__return_code__ == 0, x < y, z)].
        - [#pragma expect_exit(<assertion expression>, ...)] specifies that there should be a {!Types.Exit} in which
            a list of assertions hold. The first matching result will be removed from further processing.
            E.g., [#pragma expect_exit(x == 1, y == __exit_code__, z == 1)].
        - [#pragma expect_abandoned(<reason as an regular expression string>, <assertion expression>, ...)]
            specifies that there should be a {!Types.Abandoned} of a particular reason in which a list of assertions
            hold. The first matching result will be removed from further processing.
            E.g., [#pragma expect_abandoned("Function .* not found", x == 1, y == 2, z == 3)].
        - [#pragma no_other_return] specifies that no other {!Types.Return} should be in the remaining results.
        - [#pragma no_other_exit] specifies that no other {!Types.Exit} should be in the remaining results.
        - [#pragma no_other_abandoned] specifies that no other {!Types.Abandoned} should be in the remaining results.
        - [#pragma no_other_results] specifies that there should no remaining results.

    In particular, [#pragma expect_return], [#pragma expect_exit], and [#pragma expect_abandoned] accept a
    comma-separated list of assertion expressions which are written similarly to C expressions. Only integer
    expressions are accepted, including identifiers which are resolved as global variables, integer constants, unary
    as well as binary operations. Two special variables [__return_code__] and [__exit_code__] are also defined,
    corresponding to the value returned by [main()] and [exit()] respectively.
*)

open MyOUnit
open Otter


(** Flags for setting up the tests. *)
type flags = {
    command_line : string list;     (** The command line to use to run the test. *)
    has_failing_assertions : bool;  (** If failing assertions are expected in the test. *)
}


(** The default test flags. *)
let default_flags = {
    command_line = [];
    has_failing_assertions = false;
}


(** CPS identity function. *)
let id = fun x k -> k x


(** CPS reverse composition. *)
let (>>>) f g = fun x k -> f x (fun x -> g x k)


(** Helper to to remove a matching item from a list. *)
let list_remove f list =
    let rec list_remove list = function
        | item::rest when f item -> Some (item, List.rev_append list rest)
        | item::rest -> list_remove (item::list) rest
        | [] -> None
    in
    list_remove [] list


(** Wrapper to assert_failure that also prints the location. *)
let assert_loc_failure file loc format =
    assert_log "%s:%d:error:@;<1 2>" file.Cil.fileName loc.Cil.line;
    assert_failure format


(** Helper to print Cil.attrparam list. *)
let attrparams_printer = list_printer Printcil.f_attrparam ",@ "


(** Helper to print Otter.Types.job_completion list. *)
let results_printer =
    let completion_printer ff = function
        | Types.Exit (exit_opt, _) -> Format.fprintf ff "Exit(@[%a@])" (option_printer To_string.bytes_ff) exit_opt
        | Types.Return (return_opt, _) -> Format.fprintf ff "Return(@[%a@])" (option_printer To_string.bytes_ff) return_opt
        | Types.Abandoned (msg, loc, _) -> Format.fprintf ff "Abandoned(\"@[%s@@%d: %s@]\")" loc.Cil.file loc.Cil.line msg
        | Types.Truncated (left, right) -> Format.fprintf ff "Truncated(...)"
    in
    list_printer completion_printer "@\n"


(** Test that an expression holds in the given result.

    Only integer constants, variables, expressions are supported. Additionally, the special variables [__return_code__]
    and [__exit_code__] correspond to the values returned from [main()] and via [exit()] respectively.
 *)
let assert_exp file loc exp result return_opt exit_opt =
    let file, state = result.Types.result_file, result.Types.result_state in

    (* translate from Cil.attrparam to bytes *)
    let rec parse_exp state = function
        | Cil.ACons ("__return_code__", []) ->
            begin match return_opt with
                | Some return -> (state, return)
                | None -> raise Exit
            end

        | Cil.ACons ("__exit_code__", []) ->
            begin match exit_opt with
                | Some exit -> (state, exit)
                | None -> raise Exit
            end

        | Cil.ACons (name, []) ->
            let varinfo_opt = try Some (Cilutility.find_global_varinfo_by_name file name) with Not_found -> None in
            begin match varinfo_opt with
                | Some varinfo ->
                    if varinfo.Cil.vtype <> Cil.intType then
                        assert_loc_failure file loc "In assertion %a: global variable %s is not an int." Printcil.f_attrparam exp name;
                    let state, lval = MemOp.state__varinfo_to_lval_block state varinfo in
                    MemOp.state__deref state (lval, (Cil.bitsSizeOf Cil.intType)/8)
                | None ->
                    assert_loc_failure file loc "In assertion %a: global variable %s not found." Printcil.f_attrparam exp name
            end

        | Cil.AInt i ->
            (state, Bytes.int_to_bytes i)

        | Cil.AUnOp (unop, exp) ->
            let state, bytes = parse_exp state exp in
            (state, Operation.of_unop unop [ (bytes, Cil.intType) ])

        | Cil.ABinOp (binop, exp1, exp2) ->
            let state, bytes1 = parse_exp state exp1 in
            let state, bytes2 = parse_exp state exp2 in
            (state, Operation.of_binop binop [ (bytes1, Cil.intType); (bytes2, Cil.intType) ])

        | exp' ->
            assert_loc_failure file loc "In assertion %a: unsupported operation %a." Printcil.f_attrparam exp Printcil.f_attrparam exp'
    in
    try
        let state, bytes = parse_exp state exp in
        let state, truth = MemOp.state__eval state state.Types.path_condition bytes in
        begin match truth with
            | Ternary.True -> true
            | Ternary.False
            | Ternary.Unknown -> false
        end
    with Exit ->
        (* __return_code__ or __exit_code__ not available *)
        false


(** Test that a list of expressions in a pragma test directive hold in the given result. *)
let assert_exps file loc exps result return_opt exit_opt =
    List.for_all (fun exp -> assert_exp file loc exp result return_opt exit_opt) exps


(** CPS test that the results contains a {!Types.Return}, passing the remaining results to the next test. *)
let expect_return file loc asserts results k =
    let asserts' = assert_exps file loc asserts in
    match list_remove (function Types.Return (return_opt, result) -> asserts' result return_opt None | _ -> false) results with
        | Some (_, results) ->
            k results
        | None when asserts = [] ->
            assert_loc_failure file loc "@[Did not find Return@\nGot:@\n  @[%a@]@]" results_printer results
        | None ->
            assert_loc_failure file loc "@[Did not find Return with assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                attrparams_printer asserts results_printer results


(** CPS test that the results contains a {!Types.Exit}, passing the remaining results to the next test. *)
let expect_exit file loc asserts results k =
    let asserts' = assert_exps file loc asserts in
    match list_remove (function Types.Exit (exit_opt, result) -> asserts' result None exit_opt | _ -> false) results with
        | Some (_, results) ->
            k results
        | None when asserts = [] ->
            assert_loc_failure file loc "@[Did not find Exit@\nGot:@\n  @[%a@]@]" results_printer results
        | None ->
            assert_loc_failure file loc "@[Did not find Exit with assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                attrparams_printer asserts results_printer results


(** CPS test that the results contains a {!Types.Abandoned} with a specific reason, passing the remaining results to the next test. *)
let expect_abandoned file loc reason asserts results k =
    let reason' = Str.regexp reason in
    let asserts' = assert_exps file loc asserts in
    let is_abandoned = function
        | Types.Abandoned (reason, loc, result) -> Str.string_match reason' reason 0 && asserts' result None None
        | _ -> false
    in
    match list_remove is_abandoned results with
        | Some (_, results) ->
            k results
        | None when asserts = [] ->
            assert_loc_failure file loc "@[Did not find Abandoned with reason:@\n  %s@\nGot:@\n  @[%a@]@]"
                reason results_printer results
        | None ->
            assert_loc_failure file loc "@[Did not find Abandoned with reason:@\n  %s@\nand assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                reason attrparams_printer asserts results_printer results


(** CPS test that there are no other {!Types.job_completion} of a particular type, passing the remaining results to the next test. *)
let no_other_x f x file loc = fun results k ->
    (* count jobs that matched f *)
    let abandoned = List.filter f results in
    if abandoned <> [] then
        assert_loc_failure file loc "@[<h2>Expected no other %s but got:@\n%a@]" x results_printer results;
    k results

(** CPS test that there are no other {!Types.Return}, passing the remaining results to the next test. *)
let no_other_return = no_other_x (function Types.Return _ -> true | _ -> false) "Return"

(** CPS test that there are no other {!Types.Exit}, passing the remaining results to the next test. *)
let no_other_exit = no_other_x (function Types.Exit _ -> true | _ -> false) "Exit"

(** CPS test that there are no other {!Types.Abandoned}, passing the remaining results to the next test. *)
let no_other_abandoned = no_other_x (function Types.Abandoned _ -> true | _ -> false) "Abandoned"

(** CPS test that there are no other {!Types.job_completion} at all *)
let no_other_results = no_other_x (fun _ -> true) "results"


(** Parse [#pragma] directives in a {!Cil.file} for test flags and expectations, and generate a test function. *)
let parse_pragmas file =
    (* get test configuration from pragmas *)
    let flags, test = Cil.foldGlobals file begin fun (flags, test as config) global -> match global with
        | Cil.GPragma (Cil.Attr (name, params), loc) ->
            begin match name, params with
                | "command_line", args ->
                    if flags.command_line <> [] then assert_loc_failure file loc "Command line already defined.";
                    let command_line = List.map begin function
                        | Cil.AStr arg -> arg
                        | _ -> assert_loc_failure file loc "Invalid command line (arguments should be \"<argument string>\")."
                    end args in
                    if command_line = [] then assert_loc_failure file loc "Invalid command line (must have at least one argument).";
                    ({ flags with command_line = command_line }, test)

                | "has_failing_assertions", [] ->
                    ({ flags with has_failing_assertions = true }, test)
                | "has_failing_assertions", _ ->
                    assert_loc_failure file loc "Invalid has_failing_assertions (should have no arguments)."

                | "expect_return", [ Cil.ACons ("", []) ] -> (* strangely, expect_return() parses to this *)
                    (flags, test >>> expect_return file loc [])
                | "expect_return", [] ->
                    assert_loc_failure file loc "Invalid expect_return (should have argument list \"expect_return(...)\")."
                | "expect_return", asserts ->
                    (flags, test >>> expect_return file loc asserts)

                | "expect_exit", [ Cil.ACons ("", []) ] -> (* strangely, expect_exit() parses to this *)
                    (flags, test >>> expect_exit file loc [])
                | "expect_exit", [] ->
                    assert_loc_failure file loc "Invalid expect_exit (should have argument list \"expect_exit(...)\")."
                | "expect_exit", asserts ->
                    (flags, test >>> expect_exit file loc asserts)

                | "expect_abandoned", (Cil.AStr reason)::asserts ->
                    (flags, test >>> expect_abandoned file loc reason asserts)
                | "expect_abandoned", _ ->
                    assert_loc_failure file loc "Invalid expect_abandoned (first argument should be a regex string to match the abandoned reason)."

                | "no_other_return", [] ->
                    (flags, test >>> no_other_return file loc)
                | "no_other_return", _ ->
                    assert_loc_failure file loc "Invalid no_other_return (should have no arguments)."

                | "no_other_exit", [] ->
                    (flags, test >>> no_other_exit file loc)
                | "no_other_exit", _ ->
                    assert_loc_failure file loc "Invalid no_other_exit (should have no arguments)."

                | "no_other_abandoned", [] ->
                    (flags, test >>> no_other_abandoned file loc)
                | "no_other_abandoned", _ ->
                    assert_loc_failure file loc "Invalid no_other_abandoned (should have no arguments)."

                | "no_other_results", [] ->
                    (flags, test >>> no_other_results file loc)
                | "no_other_results", _ ->
                    assert_loc_failure file loc "Invalid no_other_results (should have no arguments)."

                | _ ->
                    assert_loc_failure file loc "Unknown test configuration: %s(%a)." name attrparams_printer params
            end
        | _ ->
            config
    end (default_flags, id) in
    (flags, (fun results -> test results (fun _ -> ())))


(** Test helper that runs Otter on a file, using #pragmas to define test expectations.
            @param main_loop is the Otter main loop to use (default: {!Driver.init})
            @param path is the path to the file
            @return a {!TestCase} that runs Otter
*)
let test_otter_with_pragma ?(main_loop=Driver.init) path = fun () ->
    (* reset the error flag and suppress all output from the symbolic executor *)
    Errormsg.hadErrors := false;
    Executeargs.print_args.Executeargs.arg_print_nothing <- true;

    (* parse and ensure no errors *)
    let file = Frontc.parse path () in
    assert_bool "Cil parse error" (not !Errormsg.hadErrors);

    (* load the configuration from the file *)
    let flags, test = parse_pragmas file in

    (* prepare the file and run the symbolic executor *)
    Executemain.prepare_file file;
    let job = Executemain.job_for_file file flags.command_line in
    let results = main_loop job in

    (* first, test if assertions passed *)
    let log = Executedebug.get_log () in
    if not flags.has_failing_assertions then
        assert_string log;

    (* then, run the given test *)
    test results;

    (* finally, test if assertions passed *)
    if flags.has_failing_assertions then
        assert_bool "Expected some failing assertions but got none." (log <> "")
