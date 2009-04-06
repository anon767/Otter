module C = Cil

(** These are the statically-configured features. To these we append the 
  * features defined in Feature_config.ml (from Makefile) *)

let features : C.featureDescr list = 
  [ 
    (*
    Randommod.feature;
    Epicenter.feature;
    Simplify.feature;
    Canonicalize.feature;
    Callgraph.feature;
    Logwrites.feature;
    Heapify.feature1;
    Heapify.feature2;
    Oneret.feature;
     *)
    Cilly.makeCFGFeature; (* ww: make CFG *must* come before Partial *) 
    (*
    Partial.feature;
    Simplemem.feature;
    Sfi.feature;
    Dataslicing.feature;
    Logcalls.feature;
    Ptranal.feature;
    Liveness.feature;
    Inconsistency.feature;
    Randomrepair.feature;
     *)
    Executemain.feature;
    Marshal_feature.feature;
		ProgStats.feature;
  ] 
  @ Feature_config.features;;

Cilly.run features ()
