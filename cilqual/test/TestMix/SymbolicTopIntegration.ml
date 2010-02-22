open TestUtil.MyOUnit
open TestUtil.CilQualUtil
open TestUtil.MixUtil

open Mix.Feature

open Otter


(* test helper for compilation units (files) *)
let test_mix content ?(label=content) test =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "test_mix_symbolictopintegration." ".c" in
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

        let file, results = dispatch_loop (SymbolicInterpreter.exec file []) in

        (* test that no assertions failed *)
        assert_string (Executedebug.get_log ());

        test file results
    end begin fun filename ->
        Unix.unlink filename
    end

(*
 * OUnit test suite
 *)

let symbolic_only_testsuite = "Symbolic only" >::: [
    test_mix
        "int main(void) { return 0; }"
    begin fun file results ->
        assert_no_abandoned results
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
            *y = 1;
            return 0;
        }
    " begin fun file results ->
        assert_no_abandoned results
    end;

    test_mix ~label:"output argument set null locally" "
        int x = 0;
        void foo(int ** y) {
            *y = NULL;
            *y = &x;
        }
        int main(void) {
            int * z = &x;
            foo(&z);
            *z = 1;
            return 0;
        }
    " begin fun file results ->
        assert_no_abandoned results
    end;

    test_mix ~label:"(void *) output argument set null locally" "
        int x = 0;
        void foo(void ** y) {
            *y = NULL;
            *y = &x;
        }
        int main(void) {
            int * z = &x;
            foo(&z);
            *z = 1;
            return 0;
        }
    " begin fun file results ->
        assert_no_abandoned results
    end;

    test_mix ~label:"field set null locally" "
        int x = 0;
        struct a { int * i; } y = { &x };
        void foo(void) {
            y.i = NULL;
            y.i = &x;
        }
        int main(void) {
            foo();
            *y.i = 1;
            return 0;
        }
    " begin fun file results ->
        assert_no_abandoned results
    end;

    test_mix ~label:"return null" "
        int x = 0;
        int * foo(void) {
            return NULL;
        }
        int main(void) {
            int * y = foo();
            *y = 1;
            return 0;
        }
    " begin fun file results ->
        assert_has_abandoned 1 results
    end;
]

let leaf_typed_source_testsuite = "Leaf Typed Source" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed) {
                    y = &x;
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed) {
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    *y = &x;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    *y = &x;
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(typed) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(typed) {
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                struct a { int * i; } y = { &x };
                void foo(void) MIX(typed) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];
    ];

    "null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                int * y = NULL;
                void foo(void) MIX(typed) {
                    y = NULL;
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                int * y = NULL;
                void foo(void) MIX(typed) {
                    y = &x;
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                int * y = NULL;
                void foo(void) MIX(typed) {
                    y = NULL;
                    y = &x;
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    *y = &x;
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                void foo(int ** y) MIX(typed) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "(void *) output argument" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    *y = NULL;
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    *y = &x;
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                void foo(void ** y) MIX(typed) {
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                int main(void) {
                    int * z = NULL;
                    foo(&z);
                    *z = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "field" >::: [
            test_mix ~label:"set null" "
                int x = 0;
                struct a { int * i; } y = { NULL };
                void foo(void) MIX(typed) {
                    y.i = NULL;
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int x = 0;
                struct a { int * i; } y = { NULL };
                void foo(void) MIX(typed) {
                    y.i = &x;
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set null and non-null" "
                int x = 0;
                struct a { int * i; } y = { NULL };
                void foo(void) MIX(typed) {
                    if (x) {
                        y.i = NULL;
                    } else {
                        y.i = &x;
                    }
                }
                int main(void) {
                    foo();
                    *y.i = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];
    ];

    "function return" >::: [
        test_mix ~label:"null" "
            int x = 0;
            int * foo(void) MIX(typed) {
                return NULL;
            }
            int main(void) {
                int * y = foo();
                *y = 1;
                return 0;
            }
        " begin fun file results ->
            assert_has_abandoned 1 results
        end;

        test_mix ~label:"non-null" "
            int x = 0;
            int * foo(void) MIX(typed) {
                return &x;
            }
            int main(void) {
                int * y = foo();
                *y = 1;
                return 0;
            }
        " begin fun file results ->
            assert_no_abandoned results
        end;

        test_mix ~label:"null and non-null" "
            int x = 0;
            int * foo(void) MIX(typed) {
                if (x) {
                    return NULL;
                } else {
                    return &x;
                }
            }
            int main(void) {
                int * y = foo();
                *y = 1;
                return 0;
            }
        " begin fun file results ->
            assert_has_abandoned 1 results
        end;
    ];
]

let leaf_typed_sink_testsuite = "Leaf Typed Sink" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(typed) {
                    int * $(nonnull) y = x;
                }
                int main(void) {
                    x = NULL;
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(typed) {
                    int * $(nonnull) y = x;
                }
                int main(void) {
                    x = &s;
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"null on one branch, non-null on the other branch" "
                int r = 0, s = 0;
                int * x = &r;
                void foo(void) MIX(typed) {
                    int * $(nonnull) y = x;
                }
                int main(void) {
                    if (__SYMBOLIC()) {
                        x = NULL;
                    } else {
                        x = &s;
                    }
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];
    ];
]

(* exercises aliasing from symbolic blocks entering typed blocks *)
let leaf_typed_using_aliasing_testsuite = "Leaf Typed Using Aliasing" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0;
                int * x = &r;
                int * $(nonnull) y = &s;
                int ** z = &x;
                void foo(void) MIX(typed) {
                    *z = NULL;
                    y = x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0, t = 0;
                int * x = &r;
                int * $(nonnull) y = &s;
                int ** z = &x;
                void foo(void) MIX(typed) {
                    *z = &t;
                    y = x;
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 0 results
            end;
        ];
    ];
]

(* exercises aliasing from typed blocks entering symbolic blocks *)
let leaf_typed_introducing_aliasing_testsuite = "Leaf Typed Introducing Aliasing" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int r = 0, s = 0, * t = &r;
                int * x = &r;
                int ** y = &t;
                void foo(void) MIX(typed) {
                    y = &x;
                }
                int main(void) {
                    foo();
                    *y = NULL;
                    *x = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int r = 0, s = 0, * t = &r;
                int * x = &r;
                int ** y = &t;
                void foo(void) MIX(typed) {
                    y = &x;
                }
                int main(void) {
                    foo();
                    *y = &s;
                    *x = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 0 results
            end;
        ];
    ];
]

let testsuite = "SymbolicTopIntegration" >::: [
    symbolic_only_testsuite;
    leaf_typed_source_testsuite;
    leaf_typed_sink_testsuite;
    leaf_typed_using_aliasing_testsuite;
    leaf_typed_introducing_aliasing_testsuite;
]

