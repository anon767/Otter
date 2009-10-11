open TestUtil.MyOUnit
open TestUtil.CilQualUtil
open TestUtil.MixUtil

open Mix.Feature

(* test helper for compilation units (files) *)
let test_mix content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "test_mix_typedtopintegration." ".c" in
        output_string fileout (preprocess content);
        close_out fileout;
        filename
    end begin fun filename ->
        Errormsg.hadErrors := false;
        let file = Frontc.parse filename () in
        assert_bool "Cil parse error" (not !Errormsg.hadErrors);
        Cil.visitCilFile strip_location_visitor file;
        Rmtmps.removeUnusedTemps file;

        prepare_file file;

        let file, solution = dispatch_loop (TypedInterpreter.exec file) in
        assert_log "@[<v>";
        assert_log "@[<v2>Constraints:@ %a@]@\n" constraints_printer solution;
        assert_log "@]";

        test file solution
    end begin fun filename ->
        Unix.unlink filename
    end

(*
 * OUnit test suite
 *)

let typed_only_testsuite = "Typed only" >::: [
    test_mix
        "int main(void) { return 0; }"
    begin fun file solution ->
        assert_discrete_satisfiable solution
    end;

    test_mix ~label:"global variable set null locally" "
        int x = 0;
        int * $(nonnull) y = &x;
        void foo(void) {
            y = NULL;
            y = &x;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"output argument set null locally" "
        int x = 0;
        void foo(int ** y) {
            *y = NULL;
            *y = &x;
        }
        int main(void) {
            int * $(nonnull) z = &x;
            foo(&z);
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"(void *) output argument set null locally" "
        int x = 0;
        void foo(void ** y) {
            *y = NULL;
            *y = &x;
        }
        int main(void) {
            int * $(nonnull) z = &x;
            foo(&z);
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"field set null locally" "
        int x = 0;
        struct a { int * $(nonnull) i; } y = { &x };
        void foo(void) {
            y.i = NULL;
            y.i = &x;
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;

    test_mix ~label:"return null" "
        int x = 0;
        int * foo(void) {
            return NULL;
        }
        int main(void) {
            int * $(nonnull) y = foo();
            return 0;
        }
    " begin fun file solution ->
        assert_discrete_unsatisfiable solution
    end;
]

let leaf_symbolic_simple_path_testsuite = "Leaf Symbolic, Simple Path" >::: [
    "unannotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                    y = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    *y = NULL;
                    *y = &x;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    *y = NULL;
                    *y = &x;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "return" >::: [
            test_mix ~label:"null" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    return NULL;
                }
                int main(void) {
                    int * y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];
    ];

    "non-null annotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                int * $(nonnull) y = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                    y = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                int * $(nonnull) y = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    *y = NULL;
                    *y = &x;
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    *y = NULL;
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    *y = NULL;
                    *y = &x;
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    *y = NULL;
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                struct a { int * $(nonnull) i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null" "
                int x = 0;
                struct a { int * $(nonnull) i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;
        ];

        "return" >::: [
            test_mix ~label:"null" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    return NULL;
                }
                int main(void) {
                    int * $(nonnull) y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;
        ];
    ];
]

let leaf_symbolic_one_branch_testsuite = "Leaf Symbolic, One Branch" >::: [
    "unannotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = NULL;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = &x;
                    } else {
                        y = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = NULL;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = &x;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = NULL;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = &x;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = NULL;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }

            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = &x;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "return" >::: [
            test_mix ~label:"null on both branches" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return NULL;
                    } else {
                        return NULL;
                    }
                }
                int main(void) {
                    int * y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"null on one branch, non-null on the other branch" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return NULL;
                    } else {
                        return &x;
                    }
                }
                int main(void) {
                    int * y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;

            test_mix ~label:"non-null on both branches" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return &x;
                    } else {
                        return &x;
                    }
                }
                int main(void) {
                    int * y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];
    ];

    "non-null annotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                int * $(nonnull) y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = NULL;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                int * $(nonnull) y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                int * $(nonnull) y = &x;
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y = &x;
                    } else {
                        y = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = NULL;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                void foo(int ** y) MIX(symbolic) {
                    if (x) {
                        *y = &x;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = NULL;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                void foo(void ** y) MIX(symbolic) {
                    if (x) {
                        *y = &x;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                struct a { int * $(nonnull) i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = NULL;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }

            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                struct a { int * $(nonnull) i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                struct a { int * $(nonnull) i; } y = { &x };
                void foo(void) MIX(symbolic) {
                    if (x) {
                        y.i = &x;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];

        "return" >::: [
            test_mix ~label:"null on both branches" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return NULL;
                    } else {
                        return NULL;
                    }
                }
                int main(void) {
                    int * $(nonnull) y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"null on one branch, non-null on the other branch" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return NULL;
                    } else {
                        return &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_unsatisfiable solution
            end;

            test_mix ~label:"non-null on both branches" "
                int x = 0;
                int * foo(void) MIX(symbolic) {
                    if (x) {
                        return &x;
                    } else {
                        return &x;
                    }
                }
                int main(void) {
                    int * $(nonnull) y = foo();
                    return 0;
                }
            " begin fun file solution ->
                assert_discrete_satisfiable solution
            end;
        ];
    ];
]

let testsuite = "TypedTopIntegration" >::: [
    typed_only_testsuite;
    leaf_symbolic_simple_path_testsuite;
    leaf_symbolic_one_branch_testsuite;
]

