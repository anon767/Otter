open TestUtil.MyOUnit

(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Control
    open TypeQual
    open CilQual
    open Mix
    open TestUtil
end

let _ =
    (* disable printing line directives *)
    Cil.lineDirectiveStyle := None;
    (* enable print_CIL_Input, otherwise, for some reason, some __attribute__ are commented *)
    Cil.print_CIL_Input := true;
    (* setup CilQual *)
    CilQual.Feature.init_cil ();
    Mix.Feature.init_cil ();
    Cil.initCIL ();

    run_test_tt_main begin TestList [
        "TestTypeQual" >::: [
            TestTypeQual.QualSolver.testsuite;
        ];
        "TestCilQual" >::: [
            TestCilQual.Type.testsuite;
            TestCilQual.Expression.testsuite;
            TestCilQual.Instruction.testsuite;
            TestCilQual.Statement.testsuite;
            TestCilQual.Global.testsuite;
            TestCilQual.Integration.testsuite;
        ];
        "TestMix" >::: [
            TestMix.SymbolicTopIntegration.testsuite;
            TestMix.TypedTopIntegration.testsuite;
        ];
    ] end

