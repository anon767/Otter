open TestUtil.MyOUnit
open TestUtil.CilQualUtil
open TestUtil.MixUtil

open Mix.Feature

open Otter


(* Test helper for compilation units (files) *)
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

        let file, solution, block_errors = dispatch_loop (TypedInterpreter.exec file) in
        assert_log "@[<v>";
        assert_log "@[<v2>Constraints:@ %a@]@\n" constraints_printer solution;
        assert_log "@]";

        test file solution block_errors
    end begin fun filename ->
        Unix.unlink filename
    end

(*
 * OUnit test suite
 *)

let typed_only_testsuite = "Typed only" >::: [
    test_mix
        "int main(void) { return 0; }"
    begin fun file solution block_errors ->
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
    " begin fun file solution block_errors ->
        assert_discrete_unsatisfiable solution;
        assert_no_block_errors block_errors
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
    " begin fun file solution block_errors ->
        assert_discrete_unsatisfiable solution;
        assert_no_block_errors block_errors
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
    " begin fun file solution block_errors ->
        assert_discrete_unsatisfiable solution;
        assert_no_block_errors block_errors
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
    " begin fun file solution block_errors ->
        assert_discrete_unsatisfiable solution;
        assert_no_block_errors block_errors
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
    " begin fun file solution block_errors ->
        assert_discrete_unsatisfiable solution;
        assert_no_block_errors block_errors
    end;
]

let leaf_symbolic_switching_only_testsuite = "Leaf Symbolic, Switching Only" >::: [
    test_mix ~label:"program with function prototypes" "
        void foo(void);
        void foo(void) MIX(symbolic) {
        }
        int main(void) {
            foo();
            return 0;
        }
    " begin fun file solution block_errors ->
        assert_discrete_satisfiable solution;
        assert_no_block_errors block_errors
    end;
]

let leaf_symbolic_source_simple_path_testsuite = "Leaf Symbolic Source, Simple Path" >::: [
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ int * i; } field" >::: [
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ char c; int * i; } single field" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                struct a { char c; int * i; } y = { 'a', &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null" "
                int x = 0;
                struct a { char c; int * i; } y = { 'a', &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ int * i; } field" >::: [
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ char c; int * i; } field" >::: [
            test_mix ~label:"set null locally" "
                int x = 0;
                struct a { char c; int * $(nonnull) i; } y = { 'a', &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null" "
                int x = 0;
                struct a { char c; int * $(nonnull) i; } y = { 'a', &x };
                void foo(void) MIX(symbolic) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];
    ];
]

let leaf_symbolic_sink_simple_path_testsuite = "Leaf Symbolic Sink, Simple Path" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(symbolic) {
                    *x = 1;
                }
                int main(void) {
                    x = NULL;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(symbolic) {
                    *x = 1;
                }
                int main(void) {
                    x = &s;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null and non-null" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(symbolic) {
                    *x = 1;
                }
                int main(void) {
                    if (r) {
                        x = NULL;
                    } else {
                        x = &s;
                    }
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];

        "{ int * i; } field" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0;
                struct a { int * i; } x = { &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    x.i = NULL;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0;
                struct a { int * i; } x = { &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    x.i = &s;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null and non-null" "
                int r = 0, s = 0;
                struct a { int * i; } x = { &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    if (r) {
                        x.i = NULL;
                    } else {
                        x.i = &s;
                    }
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];

        "{ char c; int * i; } field" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0;
                struct a { char c; int * i; } x = { 'a', &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    x.i = NULL;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0;
                struct a { char c; int * i; } x = { 'a', &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    x.i = &s;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null and non-null" "
                int r = 0, s = 0;
                struct a { char c; int * i; } x = { 'a', &r };
                void foo(void) MIX(symbolic) {
                    *x.i = 1;
                }
                int main(void) {
                    if (r) {
                        x.i = NULL;
                    } else {
                        x.i = &s;
                    }
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];
    ];
]

let leaf_symbolic_source_one_branch_testsuite = "Leaf Symbolic Source, One Branch" >::: [
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ int * i; } field" >::: [
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

            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ char c; int * i; } field" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                struct a { char c; int * i; } y = { 'a', &x };
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

            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                struct a { char c; int * i; } y = { 'a', &x };
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                struct a { char c; int * i; } y = { 'a', &x };
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ int * i; } field" >::: [
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

            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];

        "{ char * c; int * i; } field" >::: [
            test_mix ~label:"set null on both branches" "
                int x = 0;
                struct a { char * c; int * $(nonnull) i; } y = { 'a', &x };
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

            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null on one branch, non-null on the other branch" "
                int x = 0;
                struct a { char * c; int * $(nonnull) i; } y = { 'a', &x };
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set non-null on both branches" "
                int x = 0;
                struct a { char * c; int * $(nonnull) i; } y = { 'a', &x };
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];
    ];
]

(* Exercises aliasing from symbolic blocks entering typed blocks *)
let two_leaf_symbolic_sources_simple_path_testsuite = "Two Leaf Symbolic Sources, Simple Path" >::: [
    "non-null annotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null transitively forward" "
                int x = 0;
                int * y = &x;
                int * $(nonnull) z = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                }
                void bar(void) MIX(symbolic) {
                    z = y;
                }
                int main(void) {
                    foo();
                    bar();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null transitively reversed" "
                int x = 0;
                int * y = &x;
                int * $(nonnull) z = &x;
                void foo(void) MIX(symbolic) {
                    z = y;
                }
                void bar(void) MIX(symbolic) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    bar();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];
    ];
]

(* Exercises fixpoint computation in typed blocks *)
let two_leaf_symbolic_source_sink_simple_path_testsuite = "Two Leaf Symbolic Source/Sink, Simple Path" >::: [
    "unannotated" >::: [
        "global variable" >::: [
            test_mix ~label:"set null transitively forward" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    y = NULL;
                }
                void bar(void) MIX(symbolic) {
                    *y = 1;
                }
                int main(void) {
                    foo();
                    bar();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set null transitively reversed" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic) {
                    *y = 1;
                }
                void bar(void) MIX(symbolic) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    bar();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_has_block_errors 1 block_errors
            end;
        ];
    ];
]

(* Exercises transitive conversion from an outer typed block through a symbolic block to an inner typed block *)
let nested_typed_source_symbolic_typed_sink_simple_path_testsuite
        = "Nested Typed-Source to Symbolic to Typed-Sink, Simple Path" >::: [
    "null" >::: [
        "global variable" >::: [
            test_mix ~label:"assigned to non-null" "
                int x = 0;
                int * y = &x;
                void bar(void) MIX(typed) {
                    int * $(nonnull) z = y;
                }
                void foo(void) MIX(symbolic) {
                    bar();
                }
                int main(void) {
                    y = NULL;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];
    ];
]

(* Exercises transitive conversion from an inner typed block through a symbolic block to an outer typed block *)
let nested_typed_sink_symbolic_typed_source_simple_path_testsuite
        = "Nested Typed-Sink to Symbolic to Typed-Source, Simple Path" >::: [
    "null" >::: [
        "global variable" >::: [
            test_mix ~label:"assigned to non-null" "
                int x = 0;
                int * y = &x;
                void bar(void) MIX(typed) {
                    y = NULL;
                }
                void foo(void) MIX(symbolic) {
                    bar();
                }
                int main(void) {
                    int * $(nonnull) z = y;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];
    ];
]

(* Exercises transitive conversion from an outer typed block through a symbolic block to an inner typed block,
 * and conversion of maybe-null pointers to qualified types *)
let nested_typed_source_one_branch_symbolic_typed_sink_testsuite
        = "Nested Typed-Source with One Branch to Symbolic to Typed-Sink" >::: [
    "pointer to null" >::: [
        "global variable" >::: [
            test_mix ~label:"assigned to pointer to non-null" "
                int * x = NULL;
                int r, * s = &r, ** y = &s;
                void bar(void) MIX(typed) {
                    int * $(nonnull) * z = y;
                }
                void foo(void) MIX(symbolic) {
                    bar();
                }
                int main(void) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];
    ];
]

(* Exercises transitive conversion from an inner typed block through a symbolic block to an outer typed block,
 * and conversion of maybe-null pointers to qualified types *)
let nested_typed_sink_symbolic_typed_source_one_branch_testsuite
        = "Nested Typed-Sink to Symbolic to Typed-Source with One Branch" >::: [
    "pointer to null" >::: [
        "global variable" >::: [
            test_mix ~label:"assigned to pointer to non-null" "
                int * x = NULL;
                int r, * s = &r, ** y = &s;
                void bar(void) MIX(typed) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                }
                void foo(void) MIX(symbolic) {
                    bar();
                }
                int main(void) {
                    int * $(nonnull) * z = y;
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_unsatisfiable solution;
                assert_no_block_errors block_errors
            end;
        ];
    ];
]

(* Exercises recursion handling for symbolic blocks nested in typed blocks:
 *
 * The basic structure of the test code leads to the call chain: main -> foo -> bar -> (foo -> bar -> foo);
 * where the parenthesized section is recursive.
 *
 * There is a pointer y that, in the first call to foo, is not null. It will be set not null again by foo before foo
 * calls bar, which in turn calls foo recursively, so y will be not null for all calls to foo or bar. Eventually, foo
 * sets y to null before returning via bar to an earlier call to foo, which y is then dereferenced, which leads
 * to a null-pointer dereference error.
 *
 * The recursion handling (e.g., a fixpoint computation) must achieve two things: it must terminate and not recurse
 * infinitely; and it must detect that if the pointer y is null upon return from the recursive call to foo, it will lead to
 * a null-pointer dereference.
 *)
let typed_symbolic_typed_sink_source_recursive_symbolic_testsuite
        = "Typed to Symbolic to Typed-Sink-Source to Recursive Symbolic" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic);
                void bar(void) MIX(typed) {
                    foo();
                }
                void foo(void) {
                    if (w++ < 2) {
                        y = &x;
                        bar();
                        *y = 1;
                    }
                    y = NULL;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set non-null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic);
                void bar(void) MIX(typed) {
                    foo();
                }
                void foo(void) {
                    if (w++ < 2) {
                        y = &x;
                        bar();
                        *y = 1;
                    }
                    y = &x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null and non-null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(symbolic);
                void bar(void) MIX(typed) {
                    foo();
                }
                void foo(void) {
                    if (w++ < 2) {
                        y = &x;
                        bar();
                        *y = 1;
                    }
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(symbolic);
                void bar(int ** y) MIX(typed) {
                    foo(y);
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        *y = &x;
                        bar(y);
                        **y = 1;
                    }
                    *y = NULL;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;

            test_mix ~label:"set non-null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(symbolic);
                void bar(int ** y) MIX(typed) {
                    foo(y);
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        *y = &x;
                        bar(y);
                        **y = 1;
                    }
                    *y = &x;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_no_block_errors block_errors
            end;

            test_mix ~label:"set null and non-null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(symbolic);
                void bar(int ** y) MIX(typed) {
                    foo(y);
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        *y = &x;
                        bar(y);
                        **y = 1;
                    }
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
            " begin fun file solution block_errors ->
                assert_discrete_satisfiable solution;
                assert_has_block_errors 1 block_errors
            end;
        ];
    ];
]

let testsuite = "TypedTopIntegration" >::: [
    typed_only_testsuite;
    leaf_symbolic_switching_only_testsuite;
    leaf_symbolic_source_simple_path_testsuite;
    leaf_symbolic_sink_simple_path_testsuite;
    leaf_symbolic_source_one_branch_testsuite;
    two_leaf_symbolic_sources_simple_path_testsuite;
    two_leaf_symbolic_source_sink_simple_path_testsuite;
    nested_typed_source_symbolic_typed_sink_simple_path_testsuite;
    nested_typed_sink_symbolic_typed_source_simple_path_testsuite;
    nested_typed_source_one_branch_symbolic_typed_sink_testsuite;
    nested_typed_sink_symbolic_typed_source_one_branch_testsuite;
    typed_symbolic_typed_sink_source_recursive_symbolic_testsuite;
]

