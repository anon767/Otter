open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OcamlUtilities
open CilUtilities
open OtterCore
open Types
open Job


(* test helper that runs the symbolic executor on a file given a source code as a string, and counts the results *)
let test_symbolic_pointers content ?label test =
    test_otter_core content ?label ~entry_function:"foo"
        begin fun results ->
            (* count jobs *)
            let return, exit, abandoned = List.fold_left begin fun (r, e, a) result -> match result with
                | Return _ -> (r + 1, e, a)
                | Exit _ ->  (r, e + 1, a)
                | Abandoned _ -> (r, e, a + 1)
                | Truncated _ -> (r, e, a)  (* ignored *)
            end (0, 0, 0) results in

            (* finally run the test *)
            test results return exit abandoned
        end


(* specialize assert helpers with printers for descriptive error messages *)
let assert_equal = assert_equal ~printer:Format.pp_print_int
let assert_at_least = assert_at_least ~printer:Format.pp_print_int
let assert_at_most = assert_at_most ~printer:Format.pp_print_int


(*
 * OUnit test suite
 *)

(* Note: the nopN(x) idiom is used to force lazy memory initialization.
    - nopN(...) prevents CIL from removing the otherwise side-effect free expression argument;
    - multiple nopN(...) are used to avoid conflating pointers due to context insensitivity in the pointer analysis.
*)

let soundness_testsuite = "Soundness" >::: [
    "Global variables" >::: [
        "z = &x; z = &y;" >::: [
            (* there should be at least 3 aliasing conditions: z == &x, z == &y, and z == NULL, regardless of order of
               occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "" ["
                            int x, y, *z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void foo(void) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                    return;
                                } else if (z == &y) {
                                    return;
                                } else if (z == 0) {
                                    return;
                                }
                                fail();
                            }
                            void main(void) {
                                z = &x;
                                z = &y;
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 3 return;
                            assert_equal 0 exit;
                        end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;" "
                int x, y, *z;
                void foo(void) {
                    if (z == 0) {
                        return;
                    } else {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                            return;
                        } else if (y == 3) {
                            return;
                        }
                    }
                    fail();
                }
                void main(void) {
                    z = &x;
                    z = &y;
                }
                " begin fun results return exit abandoned ->
                    assert_at_least 3 return;
                    assert_equal 0 exit;
                end;
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int *x, *y, z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void foo(void) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                    return;
                                } else if (x == &z && y == 0) {
                                    return;
                                } else if (x == 0 && y == &z) {
                                    return;
                                } else if (x == 0 && y == 0) {
                                    return;
                                }
                                fail();
                            }
                            void main(void) {
                                x = &z;
                                y = &z;
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 4 return;
                            assert_equal 0 exit;
                        end
                end;
        ];
    ];

    "Formal arguments" >::: [
        "z = &x; z = &y;" >::: [
            (* there should be at least 3 aliasing conditions: z == &x, z == &y, and z == NULL, regardless of order of
               occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int x, y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void foo(int *z) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                    return;
                                } else if (z == &y) {
                                    return;
                                } else if (z == 0) {
                                    return;
                                }
                                fail();
                            }
                            void main(void) {
                                foo(&x);
                                foo(&y);
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 3 return;
                            assert_equal 0 exit;
                        end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;" "
                int x, y;
                void foo(int *z) {
                    if (z == 0) {
                        return;
                    } else {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                            return;
                        } else if (y == 3) {
                            return;
                        }
                    }
                    fail();
                }
                void main(void) {
                    foo(&x);
                    foo(&y);
                }
                " begin fun results return exit abandoned ->
                    assert_at_least 3 return;
                    assert_equal 0 exit;
                end;
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void foo(int *x, int *y) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                    return;
                                } else if (x == &z && y == 0) {
                                    return;
                                } else if (x == 0 && y == &z) {
                                    return;
                                } else if (x == 0 && y == 0) {
                                    return;
                                }
                                fail();
                            }
                            void main(void) {
                                foo(&z, &z);
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 4 return;
                            assert_equal 0 exit;
                        end
                end;
        ];
    ];

    "Pointer to malloc'ed" >::: [
        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y == x, regardless of order of
           occurence *)
        "y == x" >:
            test_permutations [ "x"; "y" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    begin String.concat "; " ["
                        int *x, *y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        void foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return;
                            } else if (y == 0) {
                                return;
                            } else if (y == x) {
                                return;
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            y = x;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 3 return;
                        assert_equal 0 exit;
                    end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y == x, regardless
           of order of occurence *)
        "y == *x" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    begin String.concat "; " ["
                        int **x, *y;
                        void nop(int x) {}
                        void foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return;
                                } else if (y == 0) {
                                    return;
                                } else if (y == *x) {
                                    return;
                                }
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y = *x;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 4 return;
                        assert_equal 0 exit;
                    end
            in
            List.map test [ ("x; *x; y", "0", "0"); ("x; y; *x", "0", "nop(y);"); ("y; x; *x", "nop(y);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, *y == NULL or *y == *x,
           regardless of order of occurence *)
        "*y == *x" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        begin String.concat "; " ["
                            int **x, **y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            void foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return;
                                } else if (y == 0) {
                                    return;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return;
                                    } else if (*y == 0) {
                                        return;
                                    } else if (*y == *x) {
                                        return;
                                    }
                                }
                                fail();
                            }
                            void main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                *y = *x;
                                foo();
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 5 return;
                            assert_equal 0 exit;
                        end
                end
            end;

        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y == &x->f, regardless of order of
           occurence *)
        "y == &x->f" >:
            test_permutations [ "x"; "y" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    begin String.concat "; " ["
                        struct { char a; int f; } *x;
                        int *y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        void foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return;
                            } else {
                                if (y == 0) {
                                    return;
                                } else if (y == &x->f) {
                                    return;
                                }
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            y = &x->f;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 3 return;
                        assert_equal 0 exit;
                    end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y == &( *x)->f,
           regardless of order of occurence *)
        "y == &(*x)->f" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    begin String.concat "; " ["
                        struct { char a; int f; } **x;
                        int *y;
                        void nop(int x) {}
                        void foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return;
                                } else if (y == 0) {
                                    return;
                                } else if (y == &(*x)->f) {
                                    return;
                                }
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y = &(*x)->f;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 4 return;
                        assert_equal 0 exit;
                    end
            in
            List.map test [ ("x; *x; y", "0", "0"); ("x; y; *x", "0", "nop(y);"); ("y; x; *x", "nop(y);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, *y == NULL
           or *y == &( *x)->f, regardless of order of occurence *)
        "*y == &(*x)->f" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            int **y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            void foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return;
                                } else if (y == 0) {
                                    return;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return;
                                    } else if (*y == 0) {
                                        return;
                                    } else if (*y == &(*x)->f) {
                                        return;
                                    }
                                }
                                fail();
                            }
                            void main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                *y = &(*x)->f;
                                foo();
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 5 return;
                            assert_equal 0 exit;
                        end
                end
            end;

        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y.f == &x->f, regardless of order of
           occurence *)
        "y.f == &x->f" >:
            test_permutations [ "x"; "y.f" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    begin String.concat "; " ["
                        struct { char a; int f; } *x;
                        struct { char a; int *f; } y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        void foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return;
                            } else {
                                if (y.f == 0) {
                                    return;
                                } else if (y.f == &x->f) {
                                    return;
                                }
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            y.f = &x->f;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 3 return;
                        assert_equal 0 exit;
                    end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y.f == &( *x)->f,
           regardless of order of occurence *)
        "y.f == &(*x)->f" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    begin String.concat "; " ["
                        struct { char a; int f; } **x;
                        struct { char a; int *f; } y;
                        void nop(int x) {}
                        void foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return;
                                } else if (y.f == 0) {
                                    return;
                                } else if (y.f == &(*x)->f) {
                                    return;
                                }
                            }
                            fail();
                        }
                        void main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y.f = &(*x)->f;
                            foo();
                        }
                    "] end
                    begin fun results return exit abandoned ->
                        assert_at_least 4 return;
                        assert_equal 0 exit;
                    end
            in
            List.map test [ ("x; *x; y.f", "0", "0"); ("x; y.f; *x", "0", "nop(y.f);"); ("y.f; x; *x", "nop(y.f);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, y->f == NULL
           or y->f == &( *x)->f, regardless of order of occurence *)
        "*y.f == &(*x)->f" >:
            test_permutations ["x"; "y.f"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y.f"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            struct { char a; int **f; } y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            void foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return;
                                } else if (y.f == 0) {
                                    return;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return;
                                    } else if (*y.f == 0) {
                                        return;
                                    } else if (*y.f == &(*x)->f) {
                                        return;
                                    }
                                }
                                fail();
                            }
                            void main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y.f = malloc(sizeof(*y.f));
                                *y.f = &(*x)->f;
                                foo();
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 5 return;
                            assert_equal 0 exit;
                        end
                end
            end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, y->f == NULL
           or y->f == &( *x)->f, regardless of order of occurence *)
        "y->f == &(*x)->f" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "y->f"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            struct { char a; int *f; } *y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            void foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return;
                                } else if (y == 0) {
                                    return;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return;
                                    } else if (y->f == 0) {
                                        return;
                                    } else if (y->f == &(*x)->f) {
                                        return;
                                    }
                                }
                                fail();
                            }
                            void main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                y->f = &(*x)->f;
                                foo();
                            }
                        "] end
                        begin fun results return exit abandoned ->
                            assert_at_least 5 return;
                            assert_equal 0 exit;
                        end
                end
            end;

        (* empty array fields should not be treated as pointers *)
        test_symbolic_pointers ~label:"empty array field"
            begin String.concat "; " ["
                struct { int *f; int g[]; } x;
                void foo(void) {
                    if (x.f == 0) {
                        return;
                    } else {
                        return;
                    }
                    fail();
                }
                void main(void) {
                    x.f = malloc(sizeof(int));
                    foo();
                }
            "] end
            begin fun results return exit abandoned ->
                assert_at_least 2 return;
                assert_equal 0 exit;
            end;

        (* CIL automatically transforms empty array parameters into pointers *)
        test_symbolic_pointers ~label:"empty array parameter"
            begin String.concat "; " ["
                void foo(int x[]) {
                    if (x == 0) {
                        return;
                    } else {
                        return;
                    }
                    fail();
                }
                void main(void) {
                    foo(malloc(sizeof(int)));
                }
            "] end
            begin fun results return exit abandoned ->
                assert_at_least 2 return;
                assert_equal 0 exit;
            end;
    ];

    "Linked list" >::: [
        (* since the nodes are statically allocated and do not alias, but the pointer analysis may perform unification,
           there should be at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Static" "
            struct node { struct node * next; };
            void foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
            }
            void main(void) {
                struct node x, y, z;
                x.next = &y;
                y.next = &z;
                z.next = 0;
                foo(&x);
            }
        " begin fun results return exit abandoned ->
            assert_at_least 4 return;
            assert_equal 0 exit;
            assert_equal 0 abandoned;
        end;

        (* since the nodes are allocated on the stack and the analysis does not handle recursion, there should be
           at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Call stack" "
            struct node { struct node * next; };
            void foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
            }
            void qux(struct node * list) {
                struct node head;
                head.next = list;
                foo(&head);
            }
            void baz(struct node * list) {
                struct node head;
                head.next = list;
                qux(&head);
            }
            void bar(struct node * list) {
                struct node head;
                head.next = list;
                baz(&head);
            }
            void main(void) {
                bar(0);
            }
        " begin fun results return exit abandoned ->
            assert_at_least 4 return;
            assert_equal 0 exit;
            assert_equal 0 abandoned;
        end;

        (* since the nodes are dynamically allocated, there should be at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Dynamic" "
            struct node { struct node * next; };
            void foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
            }
            void main(void) {
                struct node * head = malloc(sizeof(struct node));
                struct node * x = head;
                for (int i = 0; i < 2; i++) {
                    x->next = malloc(sizeof(struct node));
                    x = x->next;
                }
                x->next = 0;
                foo(head);
            }
        " begin fun results return exit abandoned ->
            assert_at_least 4 return;
            assert_equal 0 exit;
            assert_equal 0 abandoned;
        end;

        (* since the nodes are allocated on the stack and the analysis does not handle recursion, there should be
           at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Recursive call stack" "
            struct node { struct node * next; };
            void foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
            }
            void bar(struct node * list, int n) {
                if (n < 3) {
                    struct node head;
                    head.next = list;
                    bar(&head, n + 1);
                } else {
                    foo(list);
                }
            }
            void main(void) {
                bar(0, 0);
            }
        " begin fun results return exit abandoned ->
            assert_at_least 4 return;
            assert_equal 0 exit;
            assert_equal 0 abandoned;
        end;
    ]
]

let testsuite = "SymbolicPointers" >::: [
    soundness_testsuite;
]

