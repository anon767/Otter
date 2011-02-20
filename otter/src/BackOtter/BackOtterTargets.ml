module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

module PathSet = Set.Make (struct
    type t = OtterCore.Decision.t list
    let compare = BackOtterUtilities.lex_compare OtterCore.Decision.compare
end)

(* TODO: generalize targets to program points, not just functions *)
type t = {
    mapping : PathSet.t FundecMap.t;
    last_failing_path : (Cil.fundec * OtterCore.Decision.t list) option;
}

let empty = {
    mapping = FundecMap.empty;
    last_failing_path = None;
}

let targets_ref = ref empty

let reset_targets () = targets_ref := empty

let get_pathset fundec =
    let targets = !targets_ref in
    if FundecMap.mem fundec targets.mapping then
        FundecMap.find fundec targets.mapping
    else PathSet.empty

let get_paths fundec = 
    PathSet.elements (get_pathset fundec)

let add_path fundec decisions =
    let targets = !targets_ref in
    let failing_paths = get_pathset fundec in
    targets_ref :=
        {
            mapping =
                if PathSet.mem decisions failing_paths then
                    invalid_arg "Duplicated failing path"
                else
                    FundecMap.add fundec (PathSet.add decisions failing_paths) targets.mapping
                ;
            last_failing_path = Some (fundec, decisions);
        }

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
    ) targets.mapping []

