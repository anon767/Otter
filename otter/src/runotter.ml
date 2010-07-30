
(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Otter
end

let _ =
    (* setup and run CilQual *)
    Cilly.run [
        Otter.Executemain.feature;
        Otter.Multiprocess.feature;
        Marshal_feature.feature;
        CilUtilities.FindFns.feature;
        CilUtilities.TraceLines.feature;
    ] ()

