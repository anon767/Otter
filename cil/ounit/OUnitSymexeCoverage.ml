open MyOUnit
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string, and calculates coverage *)
let test_coverage_job content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "OUnitSymexeJobs." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
        (* Suppress all output from the symbolic executor *)
        Executeargs.print_args.Executeargs.arg_print_nothing <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file ["a"] in
        let results = Driver.main_loop job in

        (* figure out the edges that were executed *)
        let all_edges = List.fold_left begin fun edges result ->
            EdgeSet.union edges result.result_history.edgesTaken
        end EdgeSet.empty results in
        let all_count = EdgeSet.cardinal all_edges in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        (* finally run the test *)
        test file results all_edges all_count
    end begin fun filename ->
        Unix.unlink filename
    end

(* assert_equal helper with a descriptive error message *)
let assert_edges_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of edges"


(*
 * OUnit test suite
 *)

let simple_coverage_testsuite = "Simple" >::: [
    test_coverage_job ~label:"One statement" "
        int main(int argc, char *argv[]) {
            return 0; /* 1 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count 1 all_count
    end;

    test_coverage_job ~label:"Two statements" "
        int main(int argc, char *argv[]) {
            int i;
            i = 0;     /* 1 */
            return i;  /* 2 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count 2 all_count
    end;
]

(* WARNING: this testsuite is currently fragile; it depends on the specifics of how edges are counted, and how blocks
   are introduced by Partial.calls_end_basic_blocks (+ x edges) *)
let function_calls_coverage_testsuite = "Function calls" >::: [
    test_coverage_job ~label:"foo();" "
        void foo(void) { /* return:2 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            return 0; /* 3 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count (3 + 1) all_count
    end;

    test_coverage_job ~label:"x = 1; foo();" "
        void foo(void) { /* return:3 */ }
        int main(int argc, char *argv[]) {
            int x;
            x = 1;    /* 1 */
            foo();    /* 2 */
            return 0; /* 4 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count (4 + 1) all_count
    end;

    test_coverage_job ~label:"foo(); bar();" "
        void foo(void) { /* return:2 */ }
        void bar(void) { /* return:4 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            bar();    /* 3 */
            return 0; /* 5 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count (5 + 1) all_count
    end;

    test_coverage_job ~label:"x = 1; foo(); y = 2; bar();" "
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
    " begin fun file results all_edges all_count ->
        assert_edges_count (7 + 1) all_count
    end;

    test_coverage_job ~label:"foo() { bar(); };" "
        void bar(void) { /* return:3 */ }
        void foo(void) { bar(); /* 2, return:4 */ }
        int main(int argc, char *argv[]) {
            foo();    /* 1 */
            return 0; /* 5 */
        }
    " begin fun file results all_edges all_count ->
        assert_edges_count (5 + 2) all_count
    end;
]

let testsuite = "OUnitSymexeCoverage" >::: [
    simple_coverage_testsuite;
    function_calls_coverage_testsuite;
]

