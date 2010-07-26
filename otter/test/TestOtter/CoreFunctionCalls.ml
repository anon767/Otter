open TestUtil.MyOUnit
open TestUtil.OtterUtil
open Format
open Otter
open Bytes
open Types



(*
 * OUnit test suite
 *)

let direct_calls_testsuite = "Direct calls" >:::
    (* A function call should:
            - before entry:
                - evaluate all its arguments (order undefined)
            - upon entry:
                - push a fresh stack frame with formal/local variables:
                    - formal/local variables are looked up first in the top-most stack frame, then in the global
                      frame, i.e., formal/local variables should shadow global variables
                    - multiple calls to the same or different function gets distinct, fresh stack frames
                - allocate memory blocks for each formal/local variable
                - assign to formal variables the argument values (not variable locations)
            - upon return:
                - deallocate the memory blocks for each formal/local variable
                - pop it's stack frame
                - assign it's return value, if provided

    *)

    (* Test helper that checks that:
            - the call stack contains only main() (i.e., function calls push and pop stack frames correctly);
            - the right number of global variables and variables in main(), and nothing else, are allocated
              in memory (i.e., memory blocks are allocated and deallocated appropriately);
            - specific global variables and variables in main() have the expected values (to probe other behaviors).
    *)
    let test_function_calls content ?label match_globals =
        test_otter_core content ?label
            begin function
                | [ Return (exit_opt, result) ] ->
                    (* make sure that the call stack has only main() on it *)
                    assert_equal
                        ~cmp:(list_equal (=))
                        ~printer:(list_printer pp_print_string ",@ ")
                        ~msg:"Call stack"
                        [ "main" ]
                        (List.map (fun x -> x.Cil.svar.Cil.vname) result.result_state.callstack);

                    (* check the values of specific global variables and variables in main() *)
                    let not_found, unequal = List.fold_left begin fun (not_found, unequal) (name, bytes) ->
                        let rec find = function
                            | Cil.GVarDecl (v, _)::_
                            | Cil.GVar (v, _, _)::_ when v.Cil.vname = name ->
                                let _, actual = Eval.rval result.Types.result_state (Cil.Lval (Cil.var v)) in
                                if bytes__equal actual bytes then
                                    (not_found, unequal)
                                else
                                    (not_found, (name, bytes, actual)::unequal)
                            | [] ->
                                (name::not_found, unequal)
                            | _::tail ->
                                find tail
                        in
                        find result.Types.result_file.Cil.globals
                    end ([], []) match_globals in

                    if not_found <> [] then
                        assert_log "Variables not found:@ @[%a@]@\n" (list_printer pp_print_string ",@ ") not_found;

                    if unequal <> [] then
                        assert_log "Variables with unexpected values:@ @[%a@]@\n"
                            begin list_printer begin fun ff (name, bytes, actual) ->
                                Format.fprintf ff
                                    "%s: @[@[<2>expected:@ %a@]@ @[<2>but got:@ %a@]@]@\n"
                                    name To_string.bytes_ff bytes To_string.bytes_ff actual
                            end "@\n" end
                            unequal;

                    if not_found <> [] || unequal <> [] then
                        assert_failure "";

                    (* check that the right number of global variables and variables in main() are allocated *)
                    let module VarInfoSet = Set.Make (struct type t = Cil.varinfo let compare = Pervasives.compare end) in
                    let var_set = Cil.foldGlobals result.Types.result_file begin fun var_set global -> match global with
                        | Cil.GVarDecl (v, _)
                        | Cil.GVar (v, _, _) when not (Cil.isFunctionType v.Cil.vtype) ->
                            VarInfoSet.add v var_set
                        | Cil.GFun (fundec, _) when fundec.Cil.svar.Cil.vname = "main" ->
                            List.fold_left (fun var_set v -> VarInfoSet.add v var_set)
                                var_set (List.rev_append fundec.Cil.slocals fundec.Cil.sformals)
                        | _ ->
                            var_set
                    end VarInfoSet.empty in
                    let block_count = MemoryBlockMap.fold (fun k v block_count -> block_count + 1)
                        result.result_state.block_to_bytes 0 in
                    assert_equal ~printer:pp_print_int ~msg:"Allocated blocks for variables"
                        (VarInfoSet.cardinal var_set) block_count

                | [ _ ] ->
                    assert_failure "Expected a single Return, but got another completion result"
                | [] ->
                    assert_failure "Expected a single Return, but got no completion result"
                | _ ->
                    assert_failure "Expected a single Return, but got more than one completion result"
            end
    in
    [
        test_function_calls
            ~label:"Call function"
            "int x;
            void foo(void) { x = 1; }
            int main(void) {
                foo();
                return 0;
            }"
            [ ("x", bytes__one) ];

        test_function_calls
            ~label:"Ignore return value"
            "int x;
            int foo(void) { return 1; }
            int main(void) {
                x = 2;
                foo();
                return 0;
            }"
            [ ("x", int_to_bytes 2) ];

        test_function_calls
            ~label:"Assign return value"
            "int x;
            int foo(void) { return 1; }
            int main(void) {
                x = foo() + 2;
                return 0;
            }"
            [ ("x", int_to_bytes 3) ];

         test_function_calls
            ~label:"Assign one argument"
            "int x;
            void foo(int a) { x = a + 1; }
            int main(void) {
                foo(2);
                return 0;
            }"
            [ ("x", int_to_bytes 3) ];

         test_function_calls
            ~label:"Assign one argument, and return it"
            "int x, y;
            int foo(int a) { x = a + 1; return x + 2; }
            int main(void) {
                y = foo(3) + 4;
                return 0;
            }"
            [ ("x", int_to_bytes 4); ("y", int_to_bytes 10) ];

         test_function_calls
            ~label:"Assign variable as argument and update locally"
            "int x;
            void foo(int a) { a = 1; }
            int main(void) {
                x = 2;
                foo(x);
                return 0;
            }"
            [ ("x", int_to_bytes 2); ];

         test_function_calls
            ~label:"Local variable shadows global variable"
            "int x;
            void foo(void) { int x; x = 1; }
            int main(void) {
                x = 2;
                foo();
                return 0;
            }"
            [ ("x", int_to_bytes 2); ];

         test_function_calls
            ~label:"Formal argument shadows global variable"
            "int x;
            void foo(int x) { x = 1; }
            int main(void) {
                x = 2;
                foo(3);
                return 0;
            }"
            [ ("x", int_to_bytes 2); ];

        test_function_calls
            ~label:"Assign two arguments"
            "int x, y;
            void foo(int a, int b) { x = a + 1; y = b + 2; }
            int main(void) {
                foo(3, 4);
                return 0;
            }"
            [ ("x", int_to_bytes 4); ("y", int_to_bytes 6) ];

        test_function_calls
            ~label:"Assign two arguments, and return the first"
            "int x, y;
            int foo(int a, int b) { x = b + 1; return a + 2; }
            int main(void) {
                y = foo(3, 4) + 5;
                return 0;
            }"
            [ ("x", int_to_bytes 5); ("y", int_to_bytes 10) ];

        test_function_calls
            ~label:"Assign two arguments, and return the second"
            "int x, y;
            int foo(int a, int b) { x = a + 1; return b + 2; }
            int main(void) {
                y = foo(3, 4) + 5;
                return 0;
            }"
            [ ("x", int_to_bytes 4); ("y", int_to_bytes 11) ];

        test_function_calls
            ~label:"Call two functions consecutively"
            "int x, y;
            void foo(void) { x = 1; }
            void bar(void) { y = 2; }
            int main(void) {
                foo();
                bar();
                return 0;
            }"
            [ ("x", bytes__one); ("y", int_to_bytes 2) ];

        test_function_calls
            ~label:"Call two functions consecutively, returning values"
            "int x, y;
            int foo(int a) { return a + 1; }
            int bar(int b) { return b + 2; }
            int main(void) {
                x = foo(3) + 4;
                y = bar(x) + 5;
                return 0;
            }"
            [ ("x", int_to_bytes 8); ("y", int_to_bytes 15) ];

        test_function_calls
            ~label:"Call two functions consecutively, shadowing formal arguments"
            "int x, y, z;
            void foo(int x) { y = x + 1; }
            void bar(int x) { z = x + 2; }
            int main(void) {
                x = 3;
                foo(4);
                bar(5);
                return 0;
            }"
            [ ("x", int_to_bytes 3); ("y", int_to_bytes 5); ("z", int_to_bytes 7) ];

        test_function_calls
            ~label:"Call two functions consecutively with the same variable as argument, shadowing formal arguments"
            "int x, y, z;
            void foo(int x) { y = x + 1; }
            void bar(int x) { z = x + 2; }
            int main(void) {
                x = 3;
                foo(x);
                bar(x);
                return 0;
            }"
            [ ("x", int_to_bytes 3); ("y", int_to_bytes 4); ("z", int_to_bytes 5) ];

        test_function_calls
            ~label:"Call two functions, one as argument to the other"
            "int x, y;
            void foo(int a) { x = y + a; }
            int bar(void) { y = 1; return y + 2; }
            int main(void) {
                foo(bar());
                return 0;
            }"
            [ ("x", int_to_bytes 4); ("y", int_to_bytes 1) ];

        test_function_calls
            ~label:"Call two functions, one as vararg argument to the other"
            "int x, y;
            void foo(int a, ...) { x = y + a; }
            int bar(void) { y = 1; return y + 2; }
            int main(void) {
                foo(3, bar());
                return 0;
            }"
            [ ("x", int_to_bytes 4); ("y", int_to_bytes 1) ];


        (* the following tests should check that stack frames are unique *)

        test_function_calls
            ~label:"Call one function which calls another"
            "int x, y;
            void foo(int a) { x = a + 1; }
            void bar(int b) { y = b + 2; foo(y + 3); }
            int main(void) {
                bar(4);
                return 0;
            }"
            [ ("x", int_to_bytes 10); ("y", int_to_bytes 6) ];

        test_function_calls
            ~label:"Call one function which calls another, returning values"
            "int x;
            int foo(int a) { return a + 1; }
            int bar(int b) { return foo(b + 2) + 3; }
            int main(void) {
                x = bar(4) + 5;
                return 0;
            }"
            [ ("x", int_to_bytes 15) ];

        test_function_calls
            ~label:"Call one function which calls another, shadowing formal arguments"
            "int x, y, z;
            void foo(int x) { y = x + 1; }
            void bar(int x) { z = x + 2; foo(z + 3); z = x + 4; }
            int main(void) {
                x = 5;
                bar(6);
                return 0;
            }"
            [ ("x", int_to_bytes 5); ("y", int_to_bytes 12); ("z", int_to_bytes 10) ];

        test_function_calls
            ~label:"Call one function which recursively calls itself, shadowing formal arguments"
            "int x, y, z;
            void foo(int x) {
                if (!x) { y = x + 1; foo(y + 2); y = x + 3; }
                else { z = x + 4; }
            }
            int main(void) {
                x = 5;
                foo(0);
                return 0;
            }"
            [ ("x", int_to_bytes 5); ("y", int_to_bytes 3); ("z", int_to_bytes 7) ];
    ]


let undefined_calls_testsuite = "Undefined calls" >:::
    (* Calls to undefined functions technically should never happen in a real program, since the linker would report
        errors. However, in Otter, such calls may occur due to incomplete Cil merges (the equivalent of linking),
        e.g., because standard library models are incomplete. Until Otter has a complete standard library, calls to
        undefined functions are unavoidable and will be reported as errors.
    *)
    let test_undefined_calls content ?label name =
        test_otter_core content ?label
            begin function
                | [ Abandoned (msg, loc, result) ] when msg = "Function "^name^" not found." ->
                    ()
                | [ Abandoned (msg, loc, result) ] ->
                    assert_failure "Expected a single Abandoned reporting \"Function %s not found\",@ but got Abandoned reporting %s" name msg
                | [ _ ] ->
                    assert_failure "Expected a single Abandoned, but got another completion result"
                | [] ->
                    assert_failure "Expected a single Abandoned, but got no completion result"
                | _ ->
                    assert_failure "Expected a single Abandoned, but got more than one completion result"
            end
    in
    [
        test_undefined_calls
            ~label:"Call undefined function"
            "void foo(void);
            int main(void) {
                foo();
                return 0;
            }"
            "foo";

        test_undefined_calls
            ~label:"Call undefined and undeclared function"
            "int main(void) {
                foo();
                return 0;
            }"
            "foo";
    ]


let testsuite = "CoreFunctionCalls" >::: [
    direct_calls_testsuite;
    undefined_calls_testsuite;
    (* TODO: calls through function pointers testsuite *)
]

