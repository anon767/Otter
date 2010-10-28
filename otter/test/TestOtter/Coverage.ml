open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OtterCore
open Types
open Job

(* test helper that runs the symbolic executor on a file given a source code as a string, and calculates coverage *)
let test_coverage content ?label tracked_fns test =
    test_otter content ?label
        ~setup:begin fun _ ->
            (* enable coverage tracking *)
            Executeargs.arg_edge_coverage := true;
            Executeargs.arg_block_coverage := true;
            Executeargs.arg_line_coverage := true;
            Executeargs.arg_cond_coverage := true;
            Executeargs.arg_path_coverage := true;
            (* enable tracking on given functions *)
            Executeargs.arg_fns := tracked_fns;
        end
        begin fun results ->
            (* figure out the coverage *)
            let (all_edges, all_blocks, all_lines, all_conds, all_paths_count) = List.fold_left begin fun (edges, blocks, lines, conds, paths_count) result ->
                match result with
                    | Return (_, c)
                    | Exit (_, c) ->
                        let edges = EdgeSet.union edges c.result_history.coveredEdges in
                        let blocks = StmtInfoSet.union blocks c.result_history.coveredBlocks in
                        let lines = LineSet.union lines c.result_history.coveredLines in
                        let conds = CondSet.union conds c.result_history.coveredConds in
                        (edges, blocks, lines, conds, paths_count + 1)
                    | Abandoned _
                    | Truncated _ -> (* TODO: should they be counted? *)
                        (edges, blocks, lines, conds, paths_count)
            end (EdgeSet.empty, StmtInfoSet.empty, LineSet.empty, CondSet.empty, 0) results in
            let all_edges_count = EdgeSet.cardinal all_edges in
            let all_blocks_count = StmtInfoSet.cardinal all_blocks in
            let all_lines_count = LineSet.cardinal all_lines in
            let all_conds_count = CondSet.cardinal all_conds in

            (* finally run the test *)
            test results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count
        end

(* assert_equal helper with a descriptive error message *)
let assert_edges_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of edges"
let assert_blocks_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of blocks"
let assert_lines_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of lines"
let assert_conds_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of conditionals"
let assert_paths_count = assert_equal ~printer:(fun ff -> Format.fprintf ff "%d") ~msg:"Wrong number of paths"


(*
 * OUnit test suite
 *)

let simple_coverage_testsuite = "Simple" >::: [
    test_coverage ~label:"Single block" "
        int main(int argc, char *argv[]) {
            return 0; /* 1 */
        }
    " ["main"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
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
            return i; /* 3 */
        }
    " ["main"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 3 all_blocks_count
    end;
]

let function_calls_coverage_testsuite = "Function calls" >::: [
    test_coverage ~label:"foo();" "
        void foo(void) { /* return:2 */ }
        int main(int argc, char *argv[]) {
            foo(); /* 1 */
            return 0; /* 3 */
        }
    " ["main"; "foo"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 3 all_blocks_count
    end;

    test_coverage ~label:"x = 1; foo();" "
        void foo(void) { /* return:2 */ }
        int main(int argc, char *argv[]) {
            int x;
            x = 1;
            foo(); /* 1 */
            return 0; /* 3 */
        }
    " ["main"; "foo"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 3 all_blocks_count
    end;

    test_coverage ~label:"foo(); bar();" "
        void foo(void) { /* return:2 */ }
        void bar(void) { /* return:4 */ }
        int main(int argc, char *argv[]) {
            foo(); /* 1 */
            bar(); /* 3 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 5 all_blocks_count
    end;

    test_coverage ~label:"x = 1; foo(); y = 2; bar();" "
        void foo(void) { /* return:2 */ }
        void bar(void) { /* return:4 */ }
        int main(int argc, char *argv[]) {
            int x, y;
            x = 1;
            foo(); /* 1 */
            y = 2;
            bar(); /* 3 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 5 all_blocks_count
    end;

    test_coverage ~label:"foo() { bar(); };" "
        void bar(void) { /* return:3 */ }
        void foo(void) { bar(); /* 2, return:4 */ }
        int main(int argc, char *argv[]) {
            foo(); /* 1 */
            return 0; /* 5 */
        }
    " ["main"; "foo"; "bar"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 5 all_blocks_count
    end;
]

let function_pointers_coverage_testsuite = "Function pointers" >::: [
    test_coverage ~label:"Call with symbolic index" "
        int foo0() {return 0;}
        int foo1() {return 1;}
        int foo2() {return 2;}
        int foo3() {return 3;}

        typedef int (*FP)();

        int main()
        {
            FP a[4];
            a[0] = foo0;
            a[1] = foo1;
            a[2] = foo2;
            a[3] = foo3;

            int x = a[__SYMBOLIC() % 4]();

            return 0;
        }
    " ["main"; "foo0"; "foo1"; "foo2"; "foo3"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 7 all_blocks_count
    end;

   test_coverage ~label:"Call with symbolic pointer" "
        int foo0() {return 0;}
        int foo1() {return 1;}
        int foo2() {return 2;}
        int foo3() {return 3;}

        typedef int (*FP)();

        int main()
        {
            FP a[4];
            a[0] = foo0;
            a[1] = foo1;
            a[2] = foo2;
            a[3] = foo3;

            FP x = a[__SYMBOLIC() % 4];
            x();

            return 0;
        }
    " ["main"; "foo0"; "foo1"; "foo2"; "foo3"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 7 all_blocks_count
    end;

    test_coverage ~label:"Call on subset" "
        int foo0() {return 0;}
        int foo1() {return 1;}
        int foo2() {return 2;}
        int foo3() {return 3;}

        typedef int (*FP)();

        int main()
        {
            FP a[4];
            a[0] = foo0;
            a[1] = foo1;
            a[2] = foo2;
            a[3] = foo3;

            FP x = a[__SYMBOLIC() % 3];
            x();

            return 0;
        }
    " ["main"; "foo0"; "foo1"; "foo2"; "foo3"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 6 all_blocks_count
    end;

    test_coverage ~label:"Call with broken functions." "
        int foo0(int a) {return 1;}
        int foo1(int a) {return 1;}

        typedef int (*FP)(int);

        int main()
        {
            FP a[4];
            a[0] = foo0;
            a[1] = 0;
            a[2] = foo1;

            FP x = a[__SYMBOLIC() % 4];
            x(0);

            return 0;
        }
    " ["main"; "foo0"; "foo1"]
    begin fun results all_edges all_edges_count all_blocks all_blocks_count all_lines all_lines_count all_conds all_conds_count all_paths_count ->
        assert_blocks_count 5 all_blocks_count
    end;
]

let testsuite = "Coverage" >::: [
    simple_coverage_testsuite;
    function_calls_coverage_testsuite;
    function_pointers_coverage_testsuite;
]

