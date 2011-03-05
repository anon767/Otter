(** Testing framework for Otter where tests can be written as C files, and test expectations are specified using
    [#pragma] directives in the test files themselves.

    Test expectations are given as [#pragma] directives in test files. Except for [#pragma command_line(...)],
    the directives will be interpreted in the order they are written.

    The following [#pragma] directives are understood:
        - [#pragma entry_function(<string function name>)] specifies the entry function at which to begin symbolic
            execution. If an entry function is given and not "main", pointers will be initialized via
            {!SymbolicPointers.job_for_middle}. This corresponds to Otter's [--entryfn] command-line option.
            E.g., [#pragma entry_function("foo")].
        - [#pragma command_line(<string argument>, ...)] specifies the command line arguments to be passed to
            [main()]. Ignore if [#pragma entry_function(...)] is given and not "main". This corresponds to Otter's
            [--arg] command-line option. E.g., [#pragma command_line("foo", "bar")].
        - [#pragma time_limit(<time in seconds>)] specifies the time limit for the symbolic execution to complete.
        - [#pragma cil_options(<string argument>, ...)] specifies the command line arguments to be passed to
            CIL. E.g., [#pragma cil_options("--noUseLogicalOperators")].
        - [#pragma init_malloc_zero] specifies that memory allocated by malloc should be initialized to zeros. Conversely, {e not
            providing} this directive specifies that memory allocated by malloc should be initialized to undefined values. This corresponds to Otter's
            [--initMallocZero] command-line option.
        - [#pragma init_local_zero] specifies that local variables should be initialized to zeros. Conversely, {e not
            providing} this directive specifies that local variable should be initialized to undefined values. This corresponds to Otter's
            [--initLocalZero] command-line option.
        - [#pragma no_bounds_checking] specifies that bounds checking should be disabled. Conversely, {e not
            providing} this directive specifies that bounds checking should be enabled. This corresponds to Otter's
            [--noboundsChecking] command-line option.
        - [#pragma max_steps(<integer bound>)] bounds the number of instruction steps to execute. This
            corresponds to Otter's [--max-steps] command-line option.
        - [#pragma max_paths(<integer bound>)] bounds the number of paths to execute to completion. This corresponds
            to Otter's [--max-paths] command-line option.
        - [#pragma max_abandoned(<integer bound>)] bounds the number of abandoned paths to return. This corresponds
            to Otter's [--max-abandoned] command-line option.
        - [#pragma expect_return(<assertion expression>, ...)] specifies that there should be a {!Job.Return}
            in which a list of assertions hold. The first matching result will be removed from further processing.
            E.g., [#pragma expect_return(__return_code__ == 0, x < y, z)].
        - [#pragma expect_exit(<assertion expression>, ...)] specifies that there should be a {!Job.Exit} in which
            a list of assertions hold. The first matching result will be removed from further processing.
            E.g., [#pragma expect_exit(x == 1, y == __exit_code__, z == 1)].
        - [#pragma expect_abandoned(<reason>, <assertion expression>, ...)] specifies that there should be a
            {!Job.Abandoned} of a particular reason in which a list of assertions hold. The first matching result
            will be removed from further processing. Reasons include: {ul
                {- [failure("<regular expression>")] for [`Failure msg] reason, where the argument is a regular
                    expression as a string to match [msg]. E.g.,
                    [#pragma expect_abandoned(failure("Function .* not found"), x == 1, y == 2, z == 3)].}
                {- [assertion_failure] for [`AssertionFailure exp] reason.}
                {- [out_of_bounds] for [`OutOfBounds exp] reason.}
            }
        - [#pragma no_other_return] specifies that no other {!Job.Return} should be in the remaining results.
        - [#pragma no_other_exit] specifies that no other {!Job.Exit} should be in the remaining results.
        - [#pragma no_other_abandoned] specifies that no other {!Job.Abandoned} should be in the remaining results.
        - [#pragma no_other_results] specifies that there should no remaining results.

    In particular, [#pragma expect_return], [#pragma expect_exit], and [#pragma expect_abandoned] accept a
    comma-separated list of assertion expressions which are written similarly to C expressions. Only integer
    expressions are accepted, including identifiers which are resolved as global variables, integer constants, unary
    as well as binary operations. Two special variables [__return_code__] and [__exit_code__] are also defined,
    corresponding to the value returned by [main()] and [exit()] respectively.
*)

open MyOUnit
open DataStructures
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore
open OtterReporter
open OtterDriver


module type Errors = sig
    type t
    val printer : Format.formatter -> t -> unit
    val matcher : string -> Cil.attrparam list -> t -> bool
end

module Make (Errors : Errors) = struct
    (** Flags for setting up the tests. *)
    type flags = {
        entry_function : string option; (** The function at which to begin symbolic execution (corresponds to [--entryfn]). *)
        command_line : string list;     (** The command line to use to run the test (corresponds to [--arg]). *)
        time_limit : int option;        (** The time limit for symbolic execution. *)
        cil_options : string list;      (** The command line options to pass to CIL. *)
        no_bounds_checking : bool;      (** Disable bounds checking (corresponds to [--noboundsChecking]). *)
        init_malloc_zero : bool;        (** Initialize mallocs to zeros (corresponds to [--initMallocZero]). *)
        init_local_zero : bool;         (** Initialize locals to zeros (corresponds to [--initLocalZero]). *)
        max_steps : int option;         (** Bound the number of steps in the execution tree to explore (corresponds to [--max-steps]). *)
        max_paths : int option;         (** Bound the number of paths to execute to completion (corresponds to [--max-paths]). *)
        max_abandoned : int option;     (** Bound the number of abandoned paths to return (corresponds to [--max-abandoned]). *)
    }


    (** The default test flags. *)
    let default_flags = {
        entry_function = None;
        command_line = [];
        time_limit = None;
        cil_options = [];
        no_bounds_checking = false;
        init_malloc_zero = false;
        init_local_zero = false;
        max_steps = None;
        max_paths = None;
        max_abandoned = None;
    }


    (** CPS identity function. *)
    let id = fun x k -> k x


    (** CPS reverse composition. *)
    let (>>>) f g = fun x k -> f x (fun x -> g x k)


    (** Wrapper to assert_failure that also prints the location. *)
    let assert_loc_failure loc format =
        assert_log "%s:%d:error:@;<1 2>" (Filename.basename loc.Cil.file) loc.Cil.line;
        assert_failure format


    (** Helper to print Cil.attrparam list. *)
    let attrparams_printer = list_printer Printcil.attrparam ",@ "


    (** Helper to print Otter.Job.job_completion list. *)
    let results_printer list =
        let completion_printer ff = function
            | Job.Exit (exit_opt, _) ->
                Format.fprintf ff "Exit(@[%a@])" (option_printer BytesPrinter.bytes) exit_opt
            | Job.Return (return_opt, _) ->
                Format.fprintf ff "Return(@[%a@])" (option_printer BytesPrinter.bytes) return_opt
            | Job.Abandoned (reason, job) ->
                let loc = Job.get_loc job in
                Format.fprintf ff "Abandoned(@[%s@@%d: %a@])" loc.Cil.file loc.Cil.line Errors.printer reason
            | Job.Truncated (reason, _) ->
                Format.fprintf ff "Truncated(@[%a@])" Errors.printer reason
        in
        list_printer completion_printer "@\n" list


    (** Test that an expression holds in the given result.

        Only integer constants, variables, expressions are supported. Additionally, the special variables [__return_code__]
        and [__exit_code__] correspond to the values returned from [main()] and via [exit()] respectively.
     *)
    let assert_exp file loc exp result return_opt exit_opt =
        let file = result#file in

        (* translate from Cil.attrparam to bytes *)
        let rec parse_exp result = function
            | Cil.ACons ("__return_code__", []) ->
                begin match return_opt with
                    | Some return -> (result, return)
                    | None -> raise Exit
                end

            | Cil.ACons ("__exit_code__", []) ->
                begin match exit_opt with
                    | Some exit -> (result, exit)
                    | None -> raise Exit
                end

            | Cil.ACons (name, []) ->
                let varinfo_opt = try Some (FindCil.global_varinfo_by_name file name) with Not_found -> None in
                begin match varinfo_opt with
                    | Some varinfo ->
                        if varinfo.Cil.vtype <> Cil.intType then
                            assert_loc_failure loc "In assertion %a: global variable %s is not an int." Printcil.attrparam exp name;
                        let result, lval = MemOp.state__varinfo_to_lval_block result varinfo in
                        MemOp.state__deref result (lval, (Cil.bitsSizeOf Cil.intType)/8)
                    | None ->
                        assert_loc_failure loc "In assertion %a: global variable %s not found." Printcil.attrparam exp name
                end

            | Cil.AInt i ->
                (result, Bytes.int_to_bytes i)

            | Cil.AUnOp (unop, exp) ->
                let result, bytes = parse_exp result exp in
                (result, Operator.of_unop unop [ (bytes, Cil.intType) ])

            | Cil.ABinOp (binop, exp1, exp2) ->
                let result, bytes1 = parse_exp result exp1 in
                let result, bytes2 = parse_exp result exp2 in
                (result, Operator.of_binop binop [ (bytes1, Cil.intType); (bytes2, Cil.intType) ])

            | exp' ->
                assert_loc_failure loc "In assertion %a: unsupported operation %a." Printcil.attrparam exp Printcil.attrparam exp'
        in
        try
            let result, bytes = parse_exp result exp in
            let truth = MemOp.eval result#state.State.path_condition bytes in
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


    (** CPS test that the results contains a {!Job.Return}, passing the remaining results to the next test. *)
    let expect_return file loc asserts results k =
        let asserts' = assert_exps file loc asserts in
        match ListPlus.remove_first (function Job.Return (return_opt, result) -> asserts' result return_opt None | _ -> false) results with
            | Some (_, results) ->
                k results
            | None when asserts = [] ->
                assert_loc_failure loc "@[Did not find Return@\nGot:@\n  @[%a@]@]" results_printer results
            | None ->
                assert_loc_failure loc "@[Did not find Return with assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                    attrparams_printer asserts results_printer results


    (** CPS test that the results contains a {!Job.Exit}, passing the remaining results to the next test. *)
    let expect_exit file loc asserts results k =
        let asserts' = assert_exps file loc asserts in
        match ListPlus.remove_first (function Job.Exit (exit_opt, result) -> asserts' result None exit_opt | _ -> false) results with
            | Some (_, results) ->
                k results
            | None when asserts = [] ->
                assert_loc_failure loc "@[Did not find Exit@\nGot:@\n  @[%a@]@]" results_printer results
            | None ->
                assert_loc_failure loc "@[Did not find Exit with assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                    attrparams_printer asserts results_printer results


    (** CPS test that the results contains a {!Job.Abandoned}, passing the remaining results to the next test. *)
    let expect_abandoned file loc reason args asserts =
        let reason' =
            try
                Errors.matcher reason args
            with Failure s ->
                assert_loc_failure loc "%s" s
        in
        let asserts' = assert_exps file loc asserts in
        let is_abandoned = function
            | Job.Abandoned (reason, result) -> reason' reason && asserts' result None None
            | _ -> false
        in
        fun results k -> match ListPlus.remove_first is_abandoned results with
            | Some (_, results) ->
                k results
            | None when asserts = [] ->
                assert_loc_failure loc "@[Did not find Abandoned `Failure with reason:@\n  %s %a@\nGot:@\n  @[%a@]@]"
                    reason attrparams_printer args results_printer results
            | None ->
                assert_loc_failure loc "@[Did not find Abandoned `Failure with reason:@\n  %s %a@\nand assertions:@\n  @[%a@]@\nGot:@\n  @[%a@]@]"
                    reason attrparams_printer args attrparams_printer asserts results_printer results


    (** CPS test that there are no other {!Job.job_completion} of a particular type, passing the remaining results to the next test. *)
    let no_other_x f x file loc = fun results k ->
        (* count jobs that matched f *)
        let abandoned = List.filter f results in
        if abandoned <> [] then
            assert_loc_failure loc "@[<h2>Expected no other %s but got:@\n%a@]" x results_printer results;
        k results

    (** CPS test that there are no other {!Job.Return}, passing the remaining results to the next test. *)
    let no_other_return arg =
        no_other_x (function Job.Return _ -> true | _ -> false) "Return" arg

    (** CPS test that there are no other {!Job.Exit}, passing the remaining results to the next test. *)
    let no_other_exit arg =
        no_other_x (function Job.Exit _ -> true | _ -> false) "Exit" arg

    (** CPS test that there are no other {!Job.Abandoned}, passing the remaining results to the next test. *)
    let no_other_abandoned arg =
        no_other_x (function Job.Abandoned _ -> true | _ -> false) "Abandoned" arg

    (** CPS test that there are no other {!Job.job_completion} at all *)
    let no_other_results arg = no_other_x (fun _ -> true) "results" arg


    (** Parse [#pragma] directives in a {!Cil.file} for test flags and expectations, and generate a test function. *)
    let parse_pragmas file =
        (* get test configuration from pragmas *)
        let flags, test = Cil.foldGlobals file begin fun (flags, test as config) global -> match global with
            | Cil.GPragma (Cil.Attr (name, params), loc) ->
                begin match name, params with
                    | "entry_function", [ Cil.AStr entry_function ] ->
                        if flags.entry_function <> None then assert_loc_failure loc "Entry function already defined.";
                        if entry_function = "" then assert_loc_failure loc "Invalid entry function (should not be blank).";
                        ({ flags with entry_function = Some entry_function }, test)
                    | "entry_function", _ ->
                        assert_loc_failure loc "Invalid entry function (should have exactly one string argument that is the function name)."

                    | "command_line", args ->
                        if flags.command_line <> [] then assert_loc_failure loc "Command line already defined.";
                        let command_line = List.map begin function
                            | Cil.AStr arg -> arg
                            | _ -> assert_loc_failure loc "Invalid command line (arguments should be \"<argument string>\")."
                        end args in
                        if command_line = [] then assert_loc_failure loc "Invalid command line (should have at least one argument).";
                        ({ flags with command_line = command_line }, test)

                    | "time_limit", [ Cil.AInt time_limit ] ->
                        if flags.time_limit <> None then assert_loc_failure loc "Time limit already defined.";
                        if time_limit <= 0 then assert_loc_failure loc "Invalid time limit (should be greater than 0).";
                        ({ flags with time_limit = Some time_limit }, test)
                    | "time_limit", _ ->
                        assert_loc_failure loc "Invalid time limit (should have exactly one integer argument that is the time limit in seconds)."

                    | "cil_options", args ->
                        if flags.cil_options <> [] then assert_loc_failure loc "CIL options already defined.";
                        let cil_options = List.map begin function
                            | Cil.AStr arg -> arg
                            | _ -> assert_loc_failure loc "Invalid CIL options (arguments should be \"<argument string>\")."
                        end args in
                        if cil_options = [] then assert_loc_failure loc "Invalid CIL options (should have at least one argument).";
                        ({ flags with cil_options = cil_options }, test)

                    | "no_bounds_checking", [] ->
                        ({ flags with no_bounds_checking = true }, test)
                    | "no_bounds_checking", _ ->
                        assert_loc_failure loc "Invalid no_bounds_checking (should have no arguments)."

                    | "init_malloc_zero", [] ->
                        ({ flags with init_malloc_zero = true }, test)
                    | "init_malloc_zero", _ ->
                        assert_loc_failure loc "Invalid init_malloc_zero (should have no arguments)."

                    | "init_local_zero", [] ->
                        ({ flags with init_local_zero = true }, test)
                    | "init_local_zero", _ ->
                        assert_loc_failure loc "Invalid init_local_zero (should have no arguments)."

                    | "max_steps", [ Cil.AInt max_steps ] ->
                        if flags.max_steps <> None then assert_loc_failure loc "max_steps already defined.";
                        if max_steps <= 0 then assert_loc_failure loc "Invalid max_steps bound (should be greater than 0).";
                        ({ flags with max_steps = Some max_steps }, test)
                    | "max_steps", _ ->
                        assert_loc_failure loc "Invalid max_steps (should have exactly one integer argument that is the bound)."

                    | "max_paths", [ Cil.AInt max_paths ] ->
                        if flags.max_paths <> None then assert_loc_failure loc "max_paths already defined.";
                        if max_paths <= 0 then assert_loc_failure loc "Invalid max_paths bound (should be greater than 0).";
                        ({ flags with max_paths = Some max_paths }, test)
                    | "max_paths", _ ->
                        assert_loc_failure loc "Invalid max_paths (should have exactly one integer argument that is the bound)."

                    | "max_abandoned", [ Cil.AInt max_abandoned ] ->
                        if flags.max_abandoned <> None then assert_loc_failure loc "max_abandoned already defined.";
                        if max_abandoned <= 0 then assert_loc_failure loc "Invalid max_abandoned bound (should be greater than 0).";
                        ({ flags with max_abandoned = Some max_abandoned }, test)
                    | "max_abandoned", _ ->
                        assert_loc_failure loc "Invalid max_abandoned (should have exactly one integer argument that is the bound)."

                    | "expect_return", [ Cil.ACons ("", []) ] -> (* strangely, expect_return() parses to this *)
                        (flags, test >>> expect_return file loc [])
                    | "expect_return", [] ->
                        assert_loc_failure loc "Invalid expect_return (should have argument list \"expect_return(...)\")."
                    | "expect_return", asserts ->
                        (flags, test >>> expect_return file loc asserts)

                    | "expect_exit", [ Cil.ACons ("", []) ] -> (* strangely, expect_exit() parses to this *)
                        (flags, test >>> expect_exit file loc [])
                    | "expect_exit", [] ->
                        assert_loc_failure loc "Invalid expect_exit (should have argument list \"expect_exit(...)\")."
                    | "expect_exit", asserts ->
                        (flags, test >>> expect_exit file loc asserts)

                    | "expect_abandoned", (Cil.ACons (reason, args))::asserts ->
                        (flags, test >>> expect_abandoned file loc reason args asserts)
                    | "expect_abandoned", _ ->
                        assert_loc_failure loc "Invalid expect_abandoned (first argument should be an abandoned type)."

                    | "no_other_return", [] ->
                        (flags, test >>> no_other_return file loc)
                    | "no_other_return", _ ->
                        assert_loc_failure loc "Invalid no_other_return (should have no arguments)."

                    | "no_other_exit", [] ->
                        (flags, test >>> no_other_exit file loc)
                    | "no_other_exit", _ ->
                        assert_loc_failure loc "Invalid no_other_exit (should have no arguments)."

                    | "no_other_abandoned", [] ->
                        (flags, test >>> no_other_abandoned file loc)
                    | "no_other_abandoned", _ ->
                        assert_loc_failure loc "Invalid no_other_abandoned (should have no arguments)."

                    | "no_other_results", [] ->
                        (flags, test >>> no_other_results file loc)
                    | "no_other_results", _ ->
                        assert_loc_failure loc "Invalid no_other_results (should have no arguments)."

                    | _ ->
                        assert_loc_failure loc "Unknown test configuration: %s(%a)." name attrparams_printer params
                end
            | _ ->
                config
        end (default_flags, id) in
        (flags, (fun results -> test results (fun _ -> ())))


    (** Test helper that runs Otter on a file, using #pragmas to define test expectations. A [reporter] must be
        provided to record the results.
                @param driver is the Otter main loop to use
                @param reporter is a constructor for a reporter to collect the results for additional analysis
                @param path is the path to the file
                @return a test case that runs Otter and returns a tuple of the reporter with the results, and an
                    optional exception if one was raised during the test
    *)
    let eval_otter_with_pragma driver reporter path = fun () ->
        (* reset the error flag and suppress all output from the symbolic executor *)
        Errormsg.hadErrors := false;
        Output.arg_print_mute := 1;

        (* parse and ensure no errors *)
        let file = Frontc.parse path () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* load the configuration from the file *)
        let flags, test = parse_pragmas file in

        (* set the entry function and command line *)
        begin match flags.entry_function with
            | Some fn -> ProgramPoints.set_entry fn
            | _ -> ()
        end;
        ProgramPoints.set_cli flags.command_line;

        (* disable bounds checking if required *)
        Executeargs.arg_bounds_checking := not flags.no_bounds_checking;

        (* initialize malloc memory to zeros if required *)
        Executeargs.arg_init_malloc_zero := flags.init_malloc_zero;

        (* initialize locals to zeros if required *)
        Executeargs.arg_init_local_zero := flags.init_local_zero;

        (* See if any CIL options were defined. If so, parse the file again with those options *)
        if flags.cil_options <> [] then
        Arg.parse_argv
            ~current:(ref 0)
            (Array.of_list ("otterTest"::flags.cil_options)) (* Give a fake program name, then the options *)
            Ciloptions.options (* Use these options *)
            (fun _ -> ()) (* Ignore any anonymous arguments *)
            ""; (* We don't need a help message here *)
        let file = Frontc.parse path () in

        (* set the time limit, if provided *)
        let run = match flags.time_limit with
            | Some time_limit -> assert_time_limit (float_of_int time_limit)
            | None -> fun f -> f ()
        in

        (* prepare the file and run the symbolic executor *)
        Core.prepare_file file;
        let job = OtterJob.Job.get_default file in
        let reporter = reporter ?max_steps:flags.max_steps ?max_paths:flags.max_paths ?max_abandoned:flags.max_abandoned () in
        try
            let _, reporter = run (fun () -> driver reporter job) in
            try
                (* then, run the given test *)
                test reporter#completed;
                (reporter, None)
            with e ->
                (reporter, Some e)
        with e ->
            (reporter, Some e)

    (** Creates an OUnit {!TestCase} using {!eval_otter_with_pragma} with {!BasicReporter.t} as the reporter.
                @param driver is the Otter main loop to use
                @param path is the path to the file
                @return a {!TestCase} that runs Otter
    *)
    let test_otter_with_pragma driver path () =
        match snd (eval_otter_with_pragma driver (new BasicReporter.t) path ()) with
            | Some e -> raise e
            | None -> ()
end

