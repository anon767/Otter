open MyOUnit
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string, and counts jobs that were
   merged *)
let test_merging content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "OUnitSymexeMerging." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
        (* suppress all output from the symbolic executor *)
        Executeargs.print_args.Executeargs.arg_print_nothing <- true;
        (* enable merging *)
        Executeargs.run_args.Executeargs.arg_merge_branches <- true;

        (* reset error flag *)
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);

        (* prepare the file and run the symbolic executor *)
        Executemain.prepare_file file;
        let job = Executemain.job_for_file file ["OUnitSymexeMerging"] in
        let results = Driver.main_loop job in

        (* count jobs that were merged *)
        let truncated, other =
            List.partition (function { result_completion=Types.Truncated } -> true | _ -> false) results in
        let truncated_count = List.length truncated in
        let other_count = List.length other in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        (* finally run the test *)
        test file truncated truncated_count other other_count
    end begin fun filename ->
        Unix.unlink filename
    end

(* assert_equal helper with a descriptive error message *)
let assert_job_count =
    assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of jobs"


(*
 * OUnit test suite
 *)

let one_branch_testsuite = "One branch" >::: [
    test_merging ~label:"if (a) {} else {}" "
        int main() {
            int a;
            __SYMBOLIC(&a);

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
            } /* merge:1 */
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 2 truncated_count;
        assert_job_count 1 other_count
    end;
]

let two_branches_testsuite = "Two branches" >::: [
    test_merging ~label:"if (a) {} else {} if (b) {} else {}" "
        int main() {
            int a, b;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
            } /* merge:1 */

            if (b) {
                __COMMENT(\"b\");
            } else {
                __COMMENT(\"not b\");
            } /* merge:2 */
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 4 truncated_count;
        assert_job_count 1 other_count
    end;

    test_merging ~label:"if (a) {} else {} if (a) {} else {}" "
        int main() {
            int a, a;
            __SYMBOLIC(&a);

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
            } /* merge:1 */

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
            } /* merge:2 */
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 4 truncated_count;
        assert_job_count 1 other_count
    end;

    test_merging ~label:"if (a) { if (b) {} else {} } else {}" "
        int main() {
            int a, b;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);

            if (a) {
                __COMMENT(\"a\");
                if (b) {
                    __COMMENT(\"b\");
                } else {
                    __COMMENT(\"not b \");
                } /* merge:1 */
            } else {
                __COMMENT(\"not a\");
            } /* merge:2 */
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 4 truncated_count;
        assert_job_count 1 other_count
    end;

    test_merging ~label:"if (a) {} else { if (b) {} else {} }" "
        int main() {
            int a, b;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
                if (b) {
                    __COMMENT(\"b\");
                } else {
                    __COMMENT(\"not b \");
                } /* merge:1 */
            } /* merge:2 */
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 4 truncated_count;
        assert_job_count 1 other_count
    end;
]

let many_branches_testsuite = "Many branches" >::: [
    test_merging ~label:"Complex" "
        int main(void) {
            int a, b, c, d;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);
            __SYMBOLIC(&c);
            __SYMBOLIC(&d);

            if (a) {
                __COMMENT(\"a\");
            } else {
                __COMMENT(\"not a\");
            } /* merged:1 */

            if (b) {
                __COMMENT(\"b\");
            } else {
                __COMMENT(\"not b\");
            } /* merged:2 */

            if (c) {
                __COMMENT(\"c\");
            } else if (d) {
                __COMMENT(\"d\");
            } else { /* d:merged:3 */
                __COMMENT(\"not (c or d)\");
                if (b) {
                    __COMMENT(\"not (c or d), and b\");
                } else { /* merged:4 */
                    __COMMENT(\"not (c or d), and not b\");
                } /* merged:5 */
            }
            return 0;
        }
    " begin fun file truncated truncated_count other other_count ->
        assert_log "%d, %d@\n" truncated_count other_count;
        assert_job_count 10 truncated_count;
        assert_job_count 1 other_count
    end;
]

let testsuite = "OUnitSymexeMerging" >::: [
    one_branch_testsuite;
    two_branches_testsuite;
    many_branches_testsuite;
]

