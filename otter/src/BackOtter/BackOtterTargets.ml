module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

(* TODO: also include coverage information of a target *)
type t = OtterCore.Decision.t list list FundecMap.t

let empty = FundecMap.empty

let get fundec targets =
    if FundecMap.mem fundec targets then
        FundecMap.find fundec targets
    else []

let add fundec decisions targets =
    let failing_paths = get fundec targets in
    FundecMap.add fundec (decisions :: failing_paths) targets

let is_target fundec targets =
    get fundec targets <> []

let get_fundecs targets =
    FundecMap.fold (
        fun target_fundec _ target_fundecs -> target_fundec :: target_fundecs
    ) targets []
