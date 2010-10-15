open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OcamlUtilities
open CilUtilities
open OtterCore
open Types
open Job


(* test helper that runs the symbolic executor on a file given a source code as a string, and counts jobs that were
   merged *)
let test_symbolic_pointers content ?label test =
    test_otter_core content ?label ~entry_function:"foo"
        begin fun results ->
            (* count jobs *)
            let return, exit, abandoned, truncated = List.fold_left begin fun (r, e, a, t) result -> match result with
                | Return _ -> (r + 1, e, a, t)
                | Exit _ ->  (r, e + 1, a, t)
                | Abandoned _ -> (r, e, a + 1, t)
                | Truncated _ -> (r, e, a, t + 1)
            end (0, 0, 0, 0) results in

            (* finally run the test *)
            test results return exit abandoned truncated
        end


(* specialize assert helpers with printers for descriptive error messages *)
let assert_equal = assert_equal ~printer:Format.pp_print_int
let assert_at_least = assert_at_least ~printer:Format.pp_print_int
let assert_at_most = assert_at_most ~printer:Format.pp_print_int


(*
 * OUnit test suite
 *)

let soundness_testsuite = "Soundness" >::: [
    "Global variables" >::: [
        "z = &x; z = &y;" >::: [
            (* there should be at least 3 aliasing conditions: z == &x, z == &y, and z == NULL, regardless of order of
               occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [e1; e2; e3] = List.map (fun e -> "nop(" ^ e ^ ");") permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "" ["
                            int x, y, *z;
                            void nop(void * x) {}
                            void foo(void) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                } else if (z == &y) {
                                } else if (z == 0) {
                                }
                            }
                            void main(void) {
                                z = &x;
                                z = &y;
                            }
                        "] end
                        begin fun results return exit abandoned truncated ->
                            assert_at_least return 3;
                            assert_equal exit 0;
                            assert_equal abandoned 0;
                            assert_equal truncated 0;
                        end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;" "
                int x, y, *z;
                void foo(void) {
                    if (z) {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                        } else if (y == 3) {
                        }
                    }
                }
                void main(void) {
                    z = &x;
                    z = &y;
                }
                " begin fun results return exit abandoned truncated ->
                    assert_at_least return 3;
                    assert_equal exit 0;
                    assert_equal abandoned 0;
                    assert_equal truncated 0;
                end;
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [e1; e2; e3] = List.map (fun e -> "nop(" ^ e ^ ");") permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int *x, *y, z;
                            void nop(void * x) {}
                            void foo(void) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                } else if (x == &z && y == 0) {
                                } else if (x == 0 && y == &z) {
                                } else if (x == 0 && y == 0) {
                                }
                            }
                            void main(void) {
                                x = &z;
                                y = &z;
                            }
                        "] end
                        begin fun results return exit abandoned truncated ->
                            assert_at_least return 4;
                            assert_equal exit 0;
                            assert_equal abandoned 0;
                            assert_equal truncated 0;
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
                    let [e1; e2; e3] = List.map (fun e -> "nop(" ^ e ^ ");") permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int x, y;
                            void nop(void * x) {}
                            void foo(int *z) {
                                "; e1; e2; e3; "
                                if (z == &x) {
                                } else if (z == &y) {
                                } else if (z == 0) {
                                }
                            }
                            void main(void) {
                                foo(&x);
                                foo(&y);
                            }
                        "] end
                        begin fun results return exit abandoned truncated ->
                            assert_at_least return 3;
                            assert_equal exit 0;
                            assert_equal abandoned 0;
                            assert_equal truncated 0;
                        end
                end;

            (* another way to test the 3 aliasing conditions: if z != NULL, then *z is equal x or y  *)
            test_symbolic_pointers ~label:"x = 1; y = 2; *z = 3;" "
                int x, y;
                void foo(int *z) {
                    if (z) {
                        x = 1;
                        y = 2;
                        *z = 3;
                        if (x == 3) {
                        } else if (y == 3) {
                        }
                    }
                }
                void main(void) {
                    foo(&x);
                    foo(&y);
                }
                " begin fun results return exit abandoned truncated ->
                    assert_at_least return 3;
                    assert_equal exit 0;
                    assert_equal abandoned 0;
                    assert_equal truncated 0;
                end;
        ];

        "x = &z; y = &z;" >::: [
            (* there should be at least 4 aliasing conditions: x == &z or x == NULL and y == &z or y == NULL regardless
               of order of occurrence *)
            test_permutations [ "x"; "y"; "z" ]
                begin fun permutation ->
                    let [e1; e2; e3] = List.map (fun e -> "nop(" ^ e ^ ");") permutation in
                    test_symbolic_pointers ~label:(String.concat "; " permutation)
                        begin String.concat "; " ["
                            int z;
                            void nop(void * x) {}
                            void foo(int *x, int *y) {
                                "; e1; e2; e3; "
                                if (x == &z && y == &z) {
                                } else if (x == &z && y == 0) {
                                } else if (x == 0 && y == &z) {
                                } else if (x == 0 && y == 0) {
                                }
                            }
                            void main(void) {
                                foo(&z, &z);
                            }
                        "] end
                        begin fun results return exit abandoned truncated ->
                            assert_at_least return 4;
                            assert_equal exit 0;
                            assert_equal abandoned 0;
                            assert_equal truncated 0;
                        end
                end;
        ];
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
        " begin fun results return exit abandoned truncated ->
            assert_at_least return 4;
            assert_equal exit 0;
            assert_equal abandoned 0;
            assert_equal truncated 0;
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
        " begin fun results return exit abandoned truncated ->
            assert_at_least return 4;
            assert_equal exit 0;
            assert_equal abandoned 0;
            assert_equal truncated 0;
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
        " begin fun results return exit abandoned truncated ->
            assert_at_least return 4;
            assert_equal exit 0;
            assert_equal abandoned 0;
            assert_equal truncated 0;
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
        " begin fun results return exit abandoned truncated ->
            assert_at_least return 4;
            assert_equal exit 0;
            assert_equal abandoned 0;
            assert_equal truncated 0;
        end;
    ]
]

let testsuite = "SymbolicPointers" >::: [
    soundness_testsuite;
]

