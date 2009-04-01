
(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Control
    open TypeQual
    open CilQual
end

let _ =
    (* setup and run CilQual *)
    CilQual.Feature.init_cil ();
    Cilly.run [
        CilQual.Feature.description
    ] ()

