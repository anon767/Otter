
(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open DataStructures
    open OcamlUtilities
    open OtterBytes
    open OtterCore
    open OtterJob
    open OtterQueue
    open OtterReporter
    open OtterDriver
    open OtterGraph
end

let _ =
    (* setup and run CilQual *)
    Cilly.run [
        Otter.Executemain.feature;
        MultiOtter.MultiDriver.feature;
        BackOtter.Callchain_backward.feature;
        Marshal_feature.feature;
        CilUtilities.FindFns.feature;
        CilUtilities.TraceLines.feature;
        CilUtilities.CilCallgraph.feature;
    ] ()

