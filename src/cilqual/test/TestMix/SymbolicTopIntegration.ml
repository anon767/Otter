open TestUtil.MyOUnit
open TestUtil.CilQualUtil
open TestUtil.MixUtil

open Mix.Feature

open OtterCore


(* Test helper for compilation units (files) *)
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
        int * y = &x;
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

let leaf_typed_transitive_testsuite = "Leaf Typed Transitive" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"as-is" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null" "
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    y = NULL;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "local variable" >::: [
            test_mix ~label:"as-is" "
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = &x;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null" "
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = &x;
                    y = NULL;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"as-is" "
                void foo(void * x) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = &x;
                    foo(y);
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null" "
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = &x;
                    y = NULL;
                    foo(y);
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];
    ];

    "null" >::: [
        "global variable" >::: [
            test_mix ~label:"as-is" "
                int x = 0;
                int * y = NULL;
                void foo(void) MIX(typed) {
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
                }
                int main(void) {
                    y = &x;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;
        ];

        "local variable" >::: [
            test_mix ~label:"as-is" "
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = NULL;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                void foo(void) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = NULL;
                    y = &x;
                    foo();
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"as-is" "
                void foo(void * x) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = NULL;
                    foo(y);
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                void foo(void * x) MIX(typed) {
                }
                int main(void) {
                    int x = 0;
                    int * y = NULL;
                    y = &x;
                    foo(y);
                    *y = 1;
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;
        ];
    ];
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

(* Exercises aliasing from symbolic blocks entering typed blocks *)
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

(* Exercises aliasing from typed blocks entering symbolic blocks *)
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

(* Exercises recursion handling for typed blocks nested in symbolic blocks:
 *
 * The basic structure of the test code leads to the call chain: main -> foo -> bar -> (foo -> bar -> foo);
 * where the parenthesized section is recursive.
 *
 * There is a pointer y that, in the first call to foo, is not null, and will be not null in the first call to bar.
 * It will be set not null again by bar before bar calls foo, so y will be not null for the second and all subsequent
 * calls to foo. However, foo calls bar again which, in some tests, then sets y to null before returning to foo.
 * After the second bar and the second foo return, y is null in the first bar after the foo call-site. The first bar
 * then dereferences y, which leads to a null-pointer dereference error.
 *
 * The recursion handling (e.g., a fixpoint computation) must achieve two things: it must terminate and not recurse
 * infinitely; and it must detect that if the pointer y is null upon return from the recursive call to foo, it will lead to
 * a null-pointer dereference.
 *)
let symbolic_typed_symbolic_sink_source_recursive_typed_testsuite
        = "Symbolic to Typed to Symbolic-Sink-Source to Recursive Typed" >::: [
    "non-null" >::: [
        "global variable" >::: [
            test_mix ~label:"set null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed);
                void bar(void) MIX(symbolic) {
                    y = &x;
                    foo();
                    *y = 1;
                    y = NULL;
                }
                void foo(void) {
                    if (w++ < 2) {
                        bar();
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed);
                void bar(void) MIX(symbolic) {
                    y = &x;
                    foo();
                    *y = 1;
                    y = &x;
                }
                void foo(void) {
                    if (w++ < 2) {
                        bar();
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int w = 0;
                int x = 0;
                int * y = &x;
                void foo(void) MIX(typed);
                void bar(void) MIX(symbolic) {
                    y = &x;
                    foo();
                    *y = 1;
                    if (x) {
                        y = NULL;
                    } else {
                        y = &x;
                    }
                }
                void foo(void) {
                    if (w++ < 2) {
                        bar();
                    }
                }
                int main(void) {
                    foo();
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];

        "output argument" >::: [
            test_mix ~label:"set null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(typed);
                void bar(int ** y) MIX(symbolic) {
                    *y = &x;
                    foo(y);
                    **y = 1;
                    *y = NULL;
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        bar(y);
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;

            test_mix ~label:"set non-null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(typed);
                void bar(int ** y) MIX(symbolic) {
                    *y = &x;
                    foo(y);
                    **y = 1;
                    *y = &x;
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        bar(y);
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file results ->
                assert_no_abandoned results
            end;

            test_mix ~label:"set null and non-null" "
                int w = 0;
                int x = 0;
                void foo(int ** y) MIX(typed);
                void bar(int ** y) MIX(symbolic) {
                    *y = &x;
                    foo(y);
                    **y = 1;
                    if (x) {
                        *y = NULL;
                    } else {
                        *y = &x;
                    }
                }
                void foo(int ** y) {
                    if (w++ < 2) {
                        bar(y);
                    }
                }
                int main(void) {
                    int * z = &x;
                    foo(&z);
                    return 0;
                }
            " begin fun file results ->
                assert_has_abandoned 1 results
            end;
        ];
    ];
]

let testsuite = "SymbolicTopIntegration" >::: [
    symbolic_only_testsuite;
    leaf_typed_transitive_testsuite;
    leaf_typed_source_testsuite;
    leaf_typed_sink_testsuite;
    leaf_typed_using_aliasing_testsuite;
    leaf_typed_introducing_aliasing_testsuite;
    symbolic_typed_symbolic_sink_source_recursive_typed_testsuite;
]

