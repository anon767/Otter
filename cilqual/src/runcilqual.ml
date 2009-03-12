
(* ocamlbuild's dynamic dependency is broken: turn the below into static dependencies *)
module OcamlbuildDependencies = struct
    open Control
    open TypeQual
    open CilQual
end

let _ = begin
    (* setup and run CilQual *)
    Cil.insertImplicitCasts := false;
    Cilly.run [
        CilQual.Feature.description
    ] ()
end

