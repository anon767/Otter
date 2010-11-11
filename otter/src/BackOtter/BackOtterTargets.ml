module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

module PathSet = Set.Make (struct
    type t = OtterCore.Decision.t list
    let compare = BackOtterUtilities.lex_compare OtterCore.Decision.compare
end)

(* TODO: also include coverage information of a target *)
type t = {
    mapping : PathSet.t FundecMap.t;
    last_failing_path : (Cil.fundec * OtterCore.Decision.t list) option;
}

let empty = {
    mapping = FundecMap.empty;
    last_failing_path = None;
}

let get_pathset fundec targets =
    if FundecMap.mem fundec targets.mapping then
        FundecMap.find fundec targets.mapping
    else PathSet.empty

let get fundec targets = PathSet.elements (get_pathset fundec targets)

let add fundec decisions targets =
    let failing_paths = get_pathset fundec targets in
    {
        mapping =
            if PathSet.mem decisions failing_paths then
                invalid_arg "Duplicated failing path"
            else
                FundecMap.add fundec (PathSet.add decisions failing_paths) targets.mapping
            ;
        last_failing_path = Some (fundec, decisions);
    }

let get_last_failing_path targets =
    { targets with last_failing_path = None }, targets.last_failing_path

let is_target fundec targets = get fundec targets <> []

let get_fundecs targets =
    FundecMap.fold (
        fun target_fundec _ target_fundecs -> target_fundec :: target_fundecs
    ) targets.mapping []
