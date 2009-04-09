open MyOUnit
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string, and calculates coverage *)
let test_coverage content ?(label=content) tracked_fns test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "OUnitSymexeCoverage." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
        (* Suppress all output from the symbolic executor *)
        Executeargs.print_args.Executeargs.arg_print_nothing <- true;
        (* enable coverage tracking *)
        Executeargs.run_args.Executeargs.arg_edge_coverage <- true;
        Executeargs.run_args.Executeargs.arg_stmt_coverage <- true;
        Executeargs.run_args.Executeargs.arg_line_coverage <- true;
        (* enable tracking on given functions *)
        Executeargs.run_args.Executeargs.arg_fns <-
            List.fold_left (fun a f -> StringSet.add f a) Executeargs.run_args.Executeargs.arg_fns tracked_fns;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file ["OUnitSymexeCoverage"] in
        let results = Driver.main_loop job in

        (* figure out the statements that were executed *)
        let (all_edges, all_stmts, all_lines) = List.fold_left begin fun (edges, stmts, lines) result ->
            match result with
                | Return (_, c)
                | Exit (_, c) ->
                    let edges = EdgeSet.union edges c.result_history.coveredEdges in
                    let stmts = IntSet.union stmts c.result_history.coveredStmts in
                    let lines = LineSet.union lines c.result_history.coveredLines in
                    (edges, stmts, lines)
                | Truncated (c, d) ->
                    let edges = EdgeSet.union (EdgeSet.union edges c.result_history.coveredEdges)
                                             d.result_history.coveredEdges in
                    let stmts = IntSet.union (IntSet.union stmts c.result_history.coveredStmts)
                                             d.result_history.coveredStmts in
                    let lines = LineSet.union (LineSet.union lines c.result_history.coveredLines)
                                             d.result_history.coveredLines in
                    (edges, stmts, lines)
                | Abandoned _ ->
                    (edges, stmts, lines)
        end (EdgeSet.empty, IntSet.empty, LineSet.empty) results in
        let all_edges_count = EdgeSet.cardinal all_edges in
        let all_stmts_count = IntSet.cardinal all_stmts in
        let all_lines_count = LineSet.cardinal all_lines in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        (* finally run the test *)
        test file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count
    end begin fun filename ->
        Unix.unlink filename
    end

(* assert_equal helper with a descriptive error message *)
let assert_stmts_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of statements"


(*
 * OUnit test suite
 *)

let simple_coverage_testsuite = "Simple" >::: [
    test_coverage ~label:"One statement" "
        int main(int argc, char *argv[]) {
            return 0; /* 1 */
        }
    " ["main"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 1 all_stmts_count
    end;

    test_coverage ~label:"Two statements" "
        int main(int argc, char *argv[]) {
            int i;
            i = 0;     /* 1 */
            return i;  /* 2 */
        }
    " ["main"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 2 all_stmts_count
    end;
]

let function_calls_coverage_testsuite = "Function calls" >::: [
    test_coverage ~label:"foo();" "
        void foo(void) { /* return:2 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            return 0; /* 3 */
        }
    " ["main"; "foo"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 3 all_stmts_count
    end;

    test_coverage ~label:"x = 1; foo();" "
        void foo(void) { /* return:3 */ }
        int main(int argc, char *argv[]) {
            int x;
            x = 1;    /* 1 */
            foo();    /* 2 */
            return 0; /* 4 */
        }
    " ["main"; "foo"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 4 all_stmts_count
    end;

    test_coverage ~label:"foo(); bar();" "
        void foo(void) { /* return:2 */ }
        void bar(void) { /* return:4 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            bar();    /* 3 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 5 all_stmts_count
    end;

    test_coverage ~label:"x = 1; foo(); y = 2; bar();" "
        void foo(void) { /* return:3 */ }
        void bar(void) { /* return:5 */ }
        int main(int argc, char *argv[]) {
            int x, y;
            x = 1;    /* 1 */
            foo();    /* 2 */
            y = 2;    /* 4 */
            bar();    /* 6 */
            return 0; /* 7 */
        }
    " ["main"; "foo"; "bar"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 7 all_stmts_count
    end;

    test_coverage ~label:"foo() { bar(); };" "
        void bar(void) { /* return:3 */ }
        void foo(void) { bar(); /* 2, return:4 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun file results all_edges all_edges_count all_stmts all_stmts_count all_lines all_lines_count ->
        assert_stmts_count 5 all_stmts_count
    end;
]

let testsuite = "OUnitSymexeCoverage" >::: [
    simple_coverage_testsuite;
    function_calls_coverage_testsuite;
]

