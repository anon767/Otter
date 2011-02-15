open OcamlUtilities

module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

class ['self] t rank_fn_constructor queue_constructor = object (_ : 'self)
    val fundec_map = FundecMap.empty
    (* Map a fundec to an example job. Required just because Cil.file is not globally available! TODO *)
    val example_jobs = FundecMap.empty

    method put job =
        Profiler.global#call "FunctionQueue.t#put" begin fun () ->
        let origin_function = BackOtterUtilities.get_origin_function job in
        let queue, example_jobs =
            try
                FundecMap.find origin_function fundec_map, example_jobs
            with Not_found -> queue_constructor (), FundecMap.add origin_function job example_jobs in
        let fundec_map =
            let queue = queue#put job in
            FundecMap.add origin_function queue fundec_map
        in
        {< fundec_map = fundec_map; example_jobs = example_jobs >}
        end

    method get =
        Profiler.global#call "FunctionQueue.t#get" begin fun () ->
        let fundecs = FundecMap.fold (fun key _ lst -> key :: lst) fundec_map [] in
        let rank_fn = rank_fn_constructor () in
        let ranked_fundecs = List.map (fun fundec -> (fundec, rank_fn (FundecMap.find fundec example_jobs))) fundecs in
        let ranked_fundecs = List.sort (fun (f1, r1) (f2, r2) -> (Pervasives.compare r2 r1)) ranked_fundecs in
        let rec get = function
            | (fundec, _) :: tail -> (
                let queue = FundecMap.find fundec fundec_map in
                match queue#get with
                | Some (queue, job) ->
                    let fundec_map = FundecMap.add fundec queue fundec_map in
                    Some ({< fundec_map = fundec_map >}, job)
                | None -> get tail
                )
            | [] -> None
        in
        get ranked_fundecs
        end
end

