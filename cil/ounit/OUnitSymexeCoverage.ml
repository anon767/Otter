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
        Executeargs.run_args.Executeargs.arg_block_coverage <- true;
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

        (* figure out the coverage *)
        let (all_edges, all_blocks, all_lines) = List.fold_left begin fun (edges, blocks, lines) result ->
            match result with
                | Return (_, c)
                | Exit (_, c) ->
                    let edges = EdgeSet.union edges c.result_history.coveredEdges in
                    let blocks = StmtInfoSet.union blocks c.result_history.coveredBlocks in
                    let lines = LineSet.union lines c.result_history.coveredLines in
                    (edges, blocks, lines)
                | Truncated (c, d) ->
                    let edges = EdgeSet.union (EdgeSet.union edges c.result_history.coveredEdges)
                                             d.result_history.coveredEdges in
                    let blocks = StmtInfoSet.union (StmtInfoSet.union blocks c.result_history.coveredBlocks)
                                             d.result_history.coveredBlocks in
                    let lines = LineSet.union (LineSet.union lines c.result_history.coveredLines)
                                             d.result_history.coveredLines in
                    (edges, blocks, lines)
                | Abandoned _ ->
                    (edges, blocks, lines)
        end (EdgeSet.empty, StmtInfoSet.empty, LineSet.empty) results in
        let all_edges_count = EdgeSet.cardinal all_edges in
        let all_blocks_count = StmtInfoSet.cardinal all_blocks in
        let all_lines_count = LineSet.cardinal all_lines in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        (* finally run the test *)
        test file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count
    end begin fun filename ->
        Unix.unlink filename
    end

(* assert_equal helper with a descriptive error message *)
let assert_blocks_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of blocks"


(*
 * OUnit test suite
 *)

let simple_coverage_testsuite = "Simple" >::: [
    test_coverage ~label:"Single block" "
        int main(int argc, char *argv[]) {
            return 0; /* 1 */
        }
    " ["main"]
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 1 all_blocks_count
    end;

    test_coverage ~label:"If-then-else block" "
        int main(int argc, char *argv[]) {
            int i;
            if (argc) {
                i = 0; /* 1 */
            } else {
                i = 1; /* 2 */
            }
            return i;  /* 3 */
        }
    " ["main"]
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 3 all_blocks_count
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
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 3 all_blocks_count
    end;

    test_coverage ~label:"x = 1; foo();" "
        void foo(void) { /* return:2 */ }
        int main(int argc, char *argv[]) {
            int x;
            x = 1;
            foo();    /* 1 */
            return 0; /* 3 */
        }
    " ["main"; "foo"]
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 3 all_blocks_count
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
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 5 all_blocks_count
    end;

    test_coverage ~label:"x = 1; foo(); y = 2; bar();" "
        void foo(void) { /* return:2 */ }
        void bar(void) { /* return:4 */ }
        int main(int argc, char *argv[]) {
            int x, y;
            x = 1;
            foo();    /* 1 */
            y = 2;
            bar();    /* 3 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 5 all_blocks_count
    end;

    test_coverage ~label:"foo() { bar(); };" "
        void bar(void) { /* return:3 */ }
        void foo(void) { bar(); /* 2, return:4 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun file results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count ->
        assert_blocks_count 5 all_blocks_count
    end;
]

let testsuite = "OUnitSymexeCoverage" >::: [
    simple_coverage_testsuite;
    function_calls_coverage_testsuite;
]

