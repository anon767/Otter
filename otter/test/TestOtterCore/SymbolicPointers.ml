open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore


(* test helper that runs the symbolic executor on a file given a source code as a string, and check against an expected list of results *)
let test_symbolic_pointers ?label ~expect_return ?(no_return0=false) content =
    let content = "void * malloc(unsigned long);" ^ content in
    test_otter_core content ?label ~entry_function:"foo"
        begin fun results ->
            let actual_return = List.fold_left begin fun actual_return result -> match result with
                | Job.Return (Some return, _) ->
                    begin try
                        (Bytes.bytes_to_int64_auto return)::actual_return
                    with Failure _ ->
                        assert_failure "Unexpected Return with symbolic code"
                    end
                | Job.Return _ ->
                    assert_failure "Unexpected Return with no code"
                | Job.Abandoned _ | Job.Exit _ | Job.Truncated _ ->
                    assert_failure "Unexpected Abandoned, Exit or Truncated"
            end [] results in

            let expect_return, has_return0 = List.fold_left begin fun (expect_return, has_return0) return ->
                let has_return0 = has_return0 || return = Int64.zero in
                match ListPlus.remove_first (fun x -> Int64.of_int x = return) expect_return with
                    | Some (_, expect_return) -> (expect_return, has_return0)
                    | None -> (expect_return, has_return0)
            end (expect_return, false) actual_return in

            if expect_return <> [] then
                assert_failure "Did not find Return with code: @[%a@]" (FormatPlus.pp_print_list Format.pp_print_int "@ ") expect_return;

            if no_return0 && has_return0 then
                assert_failure "Expected no Return with code 0"
        end


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
                        ~expect_return:[ 1; 2; 3 ]
                        begin String.concat "" ["
                            int x, y, *z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            int foo(void) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                    return 1;
                                } else if (z == &y) {
                                    return 2;
                                } else if (z == 0) {
                                    return 3;
                                }
                                return 0;
                            }
                            int main(void) {
                                z = &x;
                                z = &y;
                                foo();
                                return 0;
                            }
                        "] end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;"
                ~expect_return:[ 1; 2; 3 ]
                "
                int x, y, *z;
                int foo(void) {
                    if (z == 0) {
                        return 1;
                    } else {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                            return 2;
                        } else if (y == 3) {
                            return 3;
                        }
                    }
                    return 0;
                }
                int main(void) {
                    z = &x;
                    z = &y;
                    foo();
                    return 0;
                }
                ";
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        ~expect_return:[ 1; 2; 3; 4 ]
                        begin String.concat "; " ["
                            int *x, *y, z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            int foo(void) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                    return 1;
                                } else if (x == &z && y == 0) {
                                    return 2;
                                } else if (x == 0 && y == &z) {
                                    return 3;
                                } else if (x == 0 && y == 0) {
                                    return 4;
                                }
                                return 0;
                            }
                            int main(void) {
                                x = &z;
                                y = &z;
                                foo();
                                return 0;
                            }
                        "] end
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
                        ~expect_return:[ 1; 2; 3 ]
                        begin String.concat "; " ["
                            int x, y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            int foo(int *z) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                    return 1;
                                } else if (z == &y) {
                                    return 2;
                                } else if (z == 0) {
                                    return 3;
                                }
                                return 0;
                            }
                            int main(void) {
                                foo(&x);
                                foo(&y);
                                return 0;
                            }
                        "] end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;"
                ~expect_return:[ 1; 2; 3 ]
                "
                int x, y;
                int foo(int *z) {
                    if (z == 0) {
                        return 1;
                    } else {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                            return 2;
                        } else if (y == 3) {
                            return 3;
                        }
                    }
                    return 0;
                }
                int main(void) {
                    foo(&x);
                    foo(&y);
                    return 0;
                }
                ";
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [ e1; e2; e3 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; "3" ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        ~expect_return:[ 1; 2; 3; 4 ]
                        begin String.concat "; " ["
                            int z;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            int foo(int *x, int *y) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                    return 1;
                                } else if (x == &z && y == 0) {
                                    return 2;
                                } else if (x == 0 && y == &z) {
                                    return 3;
                                } else if (x == 0 && y == 0) {
                                    return 4;
                                }
                                return 0;
                            }
                            int main(void) {
                                foo(&z, &z);
                                return 0;
                            }
                        "] end
                end;
        ];
    ];

    "Pointer to malloc'ed" >::: [
        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y == x, regardless of order of
           occurence *)
        "y = x" >:
            test_permutations [ "x"; "y" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    ~expect_return:[ 1; 2; 3 ]
                    begin String.concat "; " ["
                        int *x, *y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        int foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return 1;
                            } else if (y == 0) {
                                return 2;
                            } else if (y == x) {
                                return 3;
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            y = x;
                            foo();
                            return 0;
                        }
                    "] end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y == x, regardless
           of order of occurence *)
        "y = *x" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    ~expect_return:[ 1; 2; 3; 4 ]
                    begin String.concat "; " ["
                        int **x, *y;
                        void nop(int x) {}
                        int foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return 1;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return 2;
                                } else if (y == 0) {
                                    return 3;
                                } else if (y == *x) {
                                    return 4;
                                }
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y = *x;
                            foo();
                            return 0;
                        }
                    "] end
            in
            List.map test [ ("x; *x; y", "0", "0"); ("x; y; *x", "0", "nop(y);"); ("y; x; *x", "nop(y);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, *y == NULL or *y == *x,
           regardless of order of occurence *)
        "*y = *x" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        ~expect_return:[ 1; 2; 3; 4; 5 ]
                        begin String.concat "; " ["
                            int **x, **y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            int foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return 1;
                                } else if (y == 0) {
                                    return 2;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return 3;
                                    } else if (*y == 0) {
                                        return 4;
                                    } else if (*y == *x) {
                                        return 5;
                                    }
                                }
                                return 0;
                            }
                            int main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                *y = *x;
                                foo();
                                return 0;
                            }
                        "] end
                end
            end;

        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y == &x->f, regardless of order of
           occurence *)
        "y = &x->f" >:
            test_permutations [ "x"; "y" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    ~expect_return:[ 1; 2; 3 ]
                    begin String.concat "; " ["
                        struct { char a; int f; } *x;
                        int *y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        int foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return 1;
                            } else {
                                if (y == 0) {
                                    return 2;
                                } else if (y == &x->f) {
                                    return 3;
                                }
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            y = &x->f;
                            foo();
                            return 0;
                        }
                    "] end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y == &( *x)->f,
           regardless of order of occurence *)
        "y = &(*x)->f" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    ~expect_return:[ 1; 2; 3; 4 ]
                    begin String.concat "; " ["
                        struct { char a; int f; } **x;
                        int *y;
                        void nop(int x) {}
                        int foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return 1;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return 2;
                                } else if (y == 0) {
                                    return 3;
                                } else if (y == &(*x)->f) {
                                    return 4;
                                }
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y = &(*x)->f;
                            foo();
                            return 0;
                        }
                    "] end
            in
            List.map test [ ("x; *x; y", "0", "0"); ("x; y; *x", "0", "nop(y);"); ("y; x; *x", "nop(y);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, *y == NULL
           or *y == &( *x)->f, regardless of order of occurence *)
        "*y = &(*x)->f" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        ~expect_return:[ 1; 2; 3; 4; 5 ]
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            int **y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            int foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return 1;
                                } else if (y == 0) {
                                    return 2;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return 3;
                                    } else if (*y == 0) {
                                        return 4;
                                    } else if (*y == &(*x)->f) {
                                        return 5;
                                    }
                                }
                                return 0;
                            }
                            int main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                *y = &(*x)->f;
                                foo();
                                return 0;
                            }
                        "] end
                end
            end;

        (* there should be at least 3 aliasing conditions: x == NULL, y == NULL or y.f == &x->f, regardless of order of
           occurence *)
        "y.f = &x->f" >:
            test_permutations [ "x"; "y.f" ] begin fun permutation ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation in
                test_symbolic_pointers ~label:(String.concat "; " permutation)
                    ~expect_return:[ 1; 2; 3 ]
                    begin String.concat "; " ["
                        struct { char a; int f; } *x;
                        struct { char a; int *f; } y;
                        void nop1(int x) {}
                        void nop2(int x) {}
                        int foo(void) {
                            "; e1; e2; "
                            if (x == 0) {
                                return 1;
                            } else {
                                if (y.f == 0) {
                                    return 2;
                                } else if (y.f == &x->f) {
                                    return 3;
                                }
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            y.f = &x->f;
                            foo();
                            return 0;
                        }
                    "] end
            end;

        (* there should be at least 4 aliasing conditions: x == NULL, *x == NULL, y == NULL or y.f == &( *x)->f,
           regardless of order of occurence *)
        "y.f = &(*x)->f" >: TestList begin
            let test (label, e1, e2) =
                test_symbolic_pointers ~label
                    ~expect_return:[ 1; 2; 3; 4 ]
                    begin String.concat "; " ["
                        struct { char a; int f; } **x;
                        struct { char a; int *f; } y;
                        void nop(int x) {}
                        int foo(void) {
                            "; e1; "
                            if (x == 0) {
                                return 1;
                            } else {
                                "; e2; "
                                if (*x == 0) {
                                    return 2;
                                } else if (y.f == 0) {
                                    return 3;
                                } else if (y.f == &(*x)->f) {
                                    return 4;
                                }
                            }
                            return 0;
                        }
                        int main(void) {
                            x = malloc(sizeof(*x));
                            *x = malloc(sizeof(**x));
                            y.f = &(*x)->f;
                            foo();
                            return 0;
                        }
                    "] end
            in
            List.map test [ ("x; *x; y.f", "0", "0"); ("x; y.f; *x", "0", "nop(y.f);"); ("y.f; x; *x", "nop(y.f);", "") ]
        end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, y->f == NULL
           or y->f == &( *x)->f, regardless of order of occurence *)
        "*y.f = &(*x)->f" >:
            test_permutations ["x"; "y.f"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "*y.f"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        ~expect_return:[ 1; 2; 3; 4; 5 ]
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            struct { char a; int **f; } y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            int foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return 1;
                                } else if (y.f == 0) {
                                    return 2;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return 3;
                                    } else if (*y.f == 0) {
                                        return 4;
                                    } else if (*y.f == &(*x)->f) {
                                        return 5;
                                    }
                                }
                                return 0;
                            }
                            int main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y.f = malloc(sizeof(*y.f));
                                *y.f = &(*x)->f;
                                foo();
                                return 0;
                            }
                        "] end
                end
            end;

        (* there should be at least 5 aliasing conditions: x == NULL, y == NULL, *x == NULL, y->f == NULL
           or y->f == &( *x)->f, regardless of order of occurence *)
        "y->f = &(*x)->f" >:
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["*x"; "y->f"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        ~expect_return:[ 1; 2; 3; 4; 5 ]
                        begin String.concat "; " ["
                            struct { char a; int f; } **x;
                            struct { char a; int *f; } *y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            int foo(void) {
                                "; e1; "
                                "; e2; "
                                if (x == 0) {
                                    return 1;
                                } else if (y == 0) {
                                    return 2;
                                } else {
                                    "; e3; "
                                    "; e4; "
                                    if (*x == 0) {
                                        return 3;
                                    } else if (y->f == 0) {
                                        return 4;
                                    } else if (y->f == &(*x)->f) {
                                        return 5;
                                    }
                                }
                                return 0;
                            }
                            int main(void) {
                                x = malloc(sizeof(*x));
                                *x = malloc(sizeof(**x));
                                y = malloc(sizeof(*y));
                                y->f = &(*x)->f;
                                foo();
                                return 0;
                            }
                        "] end
                end
            end;

        (* empty array fields should not be treated as pointers *)
        test_symbolic_pointers ~label:"empty array field"
            ~expect_return:[ 1; 2 ]
            begin String.concat "; " ["
                struct { int *f; int g[]; } x;
                int foo(void) {
                    if (x.f == 0) {
                        return 1;
                    } else {
                        return 2;
                    }
                    return 0;
                }
                int main(void) {
                    x.f = malloc(sizeof(int));
                    foo();
                    return 0;
                }
            "] end;

        (* CIL automatically transforms empty array parameters into pointers *)
        test_symbolic_pointers ~label:"empty array parameter"
            ~expect_return:[ 1; 2 ]
            begin String.concat "; " ["
                int foo(int x[]) {
                    if (x == 0) {
                        return 1;
                    } else {
                        return 2;
                    }
                    return 0;
                }
                int main(void) {
                    foo(malloc(sizeof(int)));
                    return 0;
                }
            "] end;
    ];

    "Linked list" >::: [
        (* since the nodes are statically allocated and do not alias, but the pointer analysis may perform unification,
           there should be at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Static"
            ~expect_return:[ 1; 1; 1; 1 ]
            "
            struct node { struct node * next; };
            int foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
                return 1;
            }
            int main(void) {
                struct node x, y, z;
                x.next = &y;
                y.next = &z;
                z.next = 0;
                foo(&x);
                return 0;
            }
        ";

        (* since the nodes are allocated on the stack and the analysis does not handle recursion, there should be
           at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Call stack"
            ~expect_return:[ 1; 1; 1; 1 ]
            "
            struct node { struct node * next; };
            int foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
                return 1;
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
            int main(void) {
                bar(0);
                return 0;
            }
        ";

        (* since the nodes are dynamically allocated, there should be at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Dynamic"
            ~expect_return:[ 1; 1; 1; 1 ]
            "
            struct node { struct node * next; };
            int foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
                return 1;
            }
            int main(void) {
                struct node * head = malloc(sizeof(struct node));
                struct node * x = head;
                for (int i = 0; i < 2; i++) {
                    x->next = malloc(sizeof(struct node));
                    x = x->next;
                }
                x->next = 0;
                foo(head);
                return 0;
            }
        ";

        (* since the nodes are allocated on the stack and the analysis does not handle recursion, there should be
           at least 4 iterations:
               list == NULL, and list of lengths 1 to 3 *)
        test_symbolic_pointers ~label:"Recursive call stack"
            ~expect_return:[ 1; 1; 1; 1 ]
            "
            struct node { struct node * next; };
            int foo(struct node * list) {
                int x = 0;
                for (struct node * el = list; el && x < 5; el = el->next, x++) {
                }
                return 1;
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
            int main(void) {
                bar(0, 0);
                return 0;
            }
        ";
    ]
]

let accuracy_testsuite = "Accuracy" >::: [
    "Pointer to global variables" >::: [
        "p = q = &x;" >:
            (* if x occurs before p or q, then the there should be exactly 3 aliasing conditions:
                    p == NULL, q == NULL, and p == q *)
            test_permutations [ "p"; "q"; ]
                begin fun permutation ->
                    let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        ~expect_return:[ 1; 2; 3 ]
                        ~no_return0:true
                        begin String.concat "" ["
                            int *p, *q, x;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop(int x) {}
                            int foo(void) {
                                nop(x);
                                "; e1; e2; "
                                if (p == 0) {
                                    return 1;
                                } else if (q == 0) {
                                    return 2;
                                } else if (p == q) {
                                    return 3;
                                }
                                return 0;
                            }
                            int main(void) {
                                p = q = &x;
                                foo();
                                return 0;
                            }
                        "] end
                end;

        "p = q = &x; p = malloc(..);" >:
            (* if x occurs before p or q, then the there should be exactly 4 aliasing conditions:
                    p == NULL, q == NULL, p == q && p == &x, and p != q && p != x *)
            test_permutations [ "p"; "q"; ]
                begin fun permutation ->
                    let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2"; ] permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        ~expect_return:[ 1; 2; 3; 4 ]
                        ~no_return0:true
                        begin String.concat "" ["
                            int *p, *q, x;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop(int x) {}
                            int foo(void) {
                                nop(x);
                                "; e1; e2; "
                                if (p == 0) {
                                    return 1;
                                } else if (q == 0) {
                                    return 2;
                                } else if (p == q && p == &x) {
                                    return 3;
                                } else if (p != q && p != &x) {
                                    return 4;
                                }
                                return 0;
                            }
                            int main(void) {
                                p = q = &x;
                                p = malloc(sizeof(int));
                                foo();
                                return 0;
                            }
                        "] end
                end;

        "p = q = &x; p = q = &y;" >:
            (* if x and y occurs before p or q, then the there should be exactly 6 aliasing conditions:
                    p == NULL, q == NULL, p == q == &x, p == q == &y, p == &x && q == &y, and p == &y && q == &x *)
            test_permutations ["x"; "y"] begin fun permutation1 ->
                let [ e1; e2 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "1"; "2" ] permutation1 in
                test_permutations ["p"; "q"] begin fun permutation2 ->
                    let [ e3; e4 ] = List.map2 (fun n e -> "nop" ^ n ^ "(" ^ e ^ ");") [ "3"; "4" ] permutation2 in
                    test_symbolic_pointers ~label:(String.concat "; " (permutation1 @ permutation2))
                        ~expect_return:[ 1; 2; 3; 4; 5; 6 ]
                        ~no_return0:true
                        begin String.concat "" ["
                            int *p, *q, x, y;
                            void nop1(int x) {}
                            void nop2(int x) {}
                            void nop3(int x) {}
                            void nop4(int x) {}
                            int foo(void) {
                                "; e1; e2; e3; e4; "
                                if (p == 0) {
                                    return 1;
                                } else if (q == 0) {
                                    return 2;
                                } else if (p == q && p == &x) {
                                    return 3;
                                } else if (p == q && p == &y) {
                                    return 4;
                                } else if (p == &x && q == &y) {
                                    return 5;
                                } else if (p == &y && q == &x) {
                                    return 6;
                                }
                                return 0;
                            }
                            int main(void) {
                                p = q = &x;
                                p = q = &y;
                                foo();
                                return 0;
                            }
                        "] end
                end
            end
    ];
]

let testsuite = "SymbolicPointers" >::: [
    soundness_testsuite;
    accuracy_testsuite;
]

