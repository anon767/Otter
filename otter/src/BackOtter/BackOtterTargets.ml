module FundecMap = Map.Make (struct
	type t = Cil.fundec
	let compare a b = let id x = x.Cil.svar.Cil.vid in Pervasives.compare (id a) (id b)
end)

type t = OtterCore.Job.decision list list FundecMap.t

let add fundec decision targets =
    let failing_paths =
        if FundecMap.mem fundec targets then
            FundecMap.find fundec targets
        else []
    in
    FundecMap.add fundec (decision :: failing_paths) targets

let mem = FundecMap.mem
let find = FundecMap.find
let empty = FundecMap.empty
