(* TODO: rename this module to PathsToTargets *)
open OtterCore
open OtterCFG

module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

module PathSet = Set.Make (DecisionPath)

type t = {
    fundec_to_pathset : PathSet.t FundecMap.t;
    last_failing_path : (Cil.fundec * DecisionPath.t) option;
}

let empty = {
    fundec_to_pathset = FundecMap.empty;
    last_failing_path = None;
}

let targets_ref = ref empty

let reset_targets () = targets_ref := empty

let get_pathset fundec =
    let targets = !targets_ref in
    if FundecMap.mem fundec targets.fundec_to_pathset then
        FundecMap.find fundec targets.fundec_to_pathset
    else PathSet.empty

let get_paths fundec = 
    PathSet.elements (get_pathset fundec)

(**
 *  @return true if [path] is a new path
 *)
let add_path fundec path =
    let targets = !targets_ref in
    let failing_paths = get_pathset fundec in
    if PathSet.mem path failing_paths then false
    else
        let _ = targets_ref :=
        {
            fundec_to_pathset = FundecMap.add fundec (PathSet.add path failing_paths) targets.fundec_to_pathset;
            last_failing_path = Some (fundec, path);
        } in
        true

let get_last_failing_path () =
    let targets = !targets_ref in
    targets_ref := { targets with last_failing_path = None };
    targets.last_failing_path

let is_target fundec = 
    get_paths fundec <> []

let get_target_fundecs () =
    let targets = !targets_ref in
    FundecMap.fold (
        fun target_fundec _ target_fundecs -> target_fundec :: target_fundecs
    ) targets.fundec_to_pathset []

