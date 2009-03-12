open OUnit
open TestCilQual

(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Control
    open TypeQual
    open CilQual
    open TestUtil
end

let _ =
    (* disable printing line directives *)
    Cil.lineDirectiveStyle := None;
    (* enable print_CIL_Input, otherwise, for some reason, some __attribute__ are commented *)
    Cil.print_CIL_Input := true;
    (* disable implicit casts *)
    Cil.insertImplicitCasts := false;

    run_test_tt_main begin "TestCilQual" >::: [
        Expression.testsuite;
        Instruction.testsuite;
        Statement.testsuite;
        Global.testsuite;
        Integration.testsuite;
    ] end

