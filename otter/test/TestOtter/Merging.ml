open TestUtil.MyOUnit
open TestUtil.OtterUtil
open Otter
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string, and counts jobs that were
   merged *)
let test_merging content ?label test =
    test_otter content ?label
        ~setup:(fun _ -> Executeargs.run_args.Executeargs.arg_merge_paths <- true)
        ~main_loop:PathMerging.init
        begin fun results ->
            (* count jobs that were merged *)
            let truncated, other = List.partition (function Types.Truncated _ -> true | _ -> false) results in
            let truncated_count = List.length truncated in
            let other_count = List.length other in
            assert_log "Truncated: %d; Other: %d@\n" truncated_count other_count;

            (* count jobs that were abandoned *)
            let abandoned = List.fold_left begin fun abandoned result -> match result with
                | Types.Abandoned (s, loc, _) -> (loc.Cil.file, loc.Cil.line, s)::abandoned
                | _ -> abandoned
            end [] results in
            if abandoned <> [] then begin
                let printer ff abandoned = ignore begin List.fold_left begin fun b (f, l, s) ->
                    Format.fprintf ff "%(%)@[%s:%d: %s@]" b f l s; "@\n"
                end "" abandoned end in
                assert_failure "@[<hv2>Abandoned paths:@\n%a@]" printer abandoned
            end;

            (* finally run the test *)
            test truncated truncated_count other other_count
        end

(* assert_equal helper with a descriptive error message *)
let assert_job_count =
    assert_equal ~printer:(fun ff (x, y) -> Format.fprintf ff "Truncated: %d; Other: %d" x y)
                 ~msg:"Wrong number of jobs"


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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (1, 1) (truncated_count, other_count)
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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (2, 1) (truncated_count, other_count)
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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (2, 1) (truncated_count, other_count)
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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (2, 1) (truncated_count, other_count)
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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (2, 1) (truncated_count, other_count)
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
    " begin fun truncated truncated_count other other_count ->
        assert_job_count (5, 1) (truncated_count, other_count)
    end;
]

let aliasing_testsuite = "Aliasing" >::: [
    test_merging ~label:"x == &a || x == &b" "
        int main(void) {
            int *x, a, b, c;
            __SYMBOLIC(&a);
			__SYMBOLIC(&b);
			__SYMBOLIC(&c);
			__ASSUME(a > 0, b > 1);

			if (c) {
				x = &a;
			} else {
				x = &b;
			}

			__ASSERT(x);
			__ASSERT(*x > 0);

			if (x == &a) {
				__ASSERT(*x == a);
			} else {
				__ASSERT(*x == b);
			}

			if (c) {
				__ASSERT(*x == a);
			} else {
				__ASSERT(*x == b);
			}

            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;

    test_merging ~label:"x == &a || x == NULL" "
        int main(void) {
            int *x, a, b;
            __SYMBOLIC(&a);
			__SYMBOLIC(&b);

			if (b) {
				x = &a;
			} else {
				x = 0;
			}

			if (x) {
				__ASSERT(*x == a);
			} else {
				__ASSERT(x == 0);
			}

			if (b) {
				__ASSERT(*x == a);
			} else {
				__ASSERT(x == 0);
			}

            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;

    test_merging ~label:"if (c) then x == (a + 2) else x == (b + 2)" "
        int main(void) {
            int *x, a[3], b[3], c;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);
            __SYMBOLIC(&c);

            if (c) {
                x = a;
            } else {
                x = b;
            }

            x++;
            x++;

            if (c) {
                __ASSERT(*x == a[2]);
            } else {
                __ASSERT(*x == b[2]);
            }
            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;

    test_merging ~label:"if (b) then x == (a + 2) else x == 0" "
        int main(void) {
            int *x, a[3], b;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);

            if (b) {
                x = a;
            } else {
                x = 0;
            }

            if (b) {
	            x++;
    	        x++;
                __ASSERT(*x == a[2]);
            } else {
				__ASSERT(x == 0);
			}
            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;

    test_merging ~label:"x == &a || (x == &b || x == NULL)" "
        int main(void) {
            int *x, a, b, c, d;
            __SYMBOLIC(&a);
            __SYMBOLIC(&b);
            __SYMBOLIC(&c);
            __SYMBOLIC(&d);

            if (c) {
                x = &a;
            } else if (d) {
                x = &b;
            } else {
                x = 0;
            }

            if (x) {
                __ASSERT(*x == a || *x == b);
            } else {
                __ASSERT(x == 0);
            }

            if (c) {
                __ASSERT(*x == a);
            } else if (d) {
                __ASSERT(*x == b);
            } else {
                __ASSERT(x == 0);
            }

            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;

    test_merging ~label:"*x, *y" "
        int main(void) {
            int *x, *y, a, b, c, d;
            __SYMBOLIC(&a);
			__SYMBOLIC(&b);
			__SYMBOLIC(&c);
			__SYMBOLIC(&d);

			if (c) {
				x = &a;
			} else {
				x = &b;
			}

			if (d) {
				y = &a;
			} else {
				y = &b;
			}

			if (!c == !d) {
				__ASSERT(x == y);
			} else {
				__ASSERT(x != y);
			}

			*x = 1;
			*y = 2;
			if (x == y) {
				__ASSERT(*x == 2);
			} else {
				__ASSERT(*x == 1);
			}

            return 0;
        }
    " begin fun truncated truncated_count other other_count ->
        ()
    end;
]

let testsuite = "Merging" >::: [
    one_branch_testsuite;
    two_branches_testsuite;
    many_branches_testsuite;
    aliasing_testsuite;
]

