
(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open DataStructures
    open OcamlUtilities
    open CilUtilities
    open OtterBytes
    open OtterCFG
    open OtterCore
    open OtterESD
    open OtterExtensions
    open OtterJob
    open OtterQueue
    open OtterReporter
    open OtterDriver
end

let _ =
    Cilly.run [
        (* Features have to be ordered by dependencies. *)
        CilUtilities.RunRmtmps.feature;
        CilUtilities.RunReachingDef.feature;
        CilUtilities.FindFns.feature;
        CilUtilities.TraceLines.feature;
        CilUtilities.CilCallgraph.feature;
        (* Features below are mutually exclusive. *)
        Otter.LinkCheck.feature;
        Otter.OtterMain.feature;
        MultiOtter.MultiDriver.feature;
        BackOtter.BackOtterMain.feature;
        DebugOtter.DebugOtterExecute.feature;
    ]

