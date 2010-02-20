
(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Otter
end

let _ =
    (* setup and run CilQual *)
    Cilly.run [
        Otter.Executemain.feature;
        Marshal_feature.feature;
        Otter.FindFns.feature;
        Otter.TraceLines.feature;
    ] ()

