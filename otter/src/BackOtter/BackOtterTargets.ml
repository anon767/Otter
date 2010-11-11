module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

(* TODO: also include coverage information of a target *)
type t = {
    mapping : OtterCore.Decision.t list list FundecMap.t;
    last_failing_path : (Cil.fundec * OtterCore.Decision.t list) option;
}

let empty = {
    mapping = FundecMap.empty;
    last_failing_path = None;
}

let get fundec targets =
    if FundecMap.mem fundec targets.mapping then
        FundecMap.find fundec targets.mapping
    else []

let add fundec decisions targets =
    let failing_paths = get fundec targets in
    {
        mapping = FundecMap.add fundec (decisions :: failing_paths) targets.mapping;
        last_failing_path = Some (fundec, decisions);
    }

let get_last_failing_path targets =
    { targets with last_failing_path = None }, targets.last_failing_path

let is_target fundec targets = get fundec targets <> []

let get_fundecs targets =
    FundecMap.fold (
        fun target_fundec _ target_fundecs -> target_fundec :: target_fundecs
    ) targets.mapping []
