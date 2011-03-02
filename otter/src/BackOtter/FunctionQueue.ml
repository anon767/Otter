open OcamlUtilities

module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

class ['self] t rank_fn queue_constructor = object (_ : 'self)
    val fundec_map = FundecMap.empty
    (* Map a fundec to the file containing it. Required just because Cil.file is not globally available! TODO *)
    val file_of_fundec = FundecMap.empty

    method put job =
        Profiler.global#call "FunctionQueue.t#put" begin fun () ->
        let origin_function = BackOtterUtilities.get_origin_function job in
        let queue, file_of_fundec =
            try
                FundecMap.find origin_function fundec_map, file_of_fundec
            with Not_found -> queue_constructor (), FundecMap.add origin_function job#file file_of_fundec in
        let fundec_map =
            let queue = queue#put job in
            FundecMap.add origin_function queue fundec_map
        in
        {< fundec_map = fundec_map; file_of_fundec = file_of_fundec >}
        end

    method get =
        Profiler.global#call "FunctionQueue.t#get" begin fun () ->
        let fundecs = FundecMap.fold (fun key _ lst -> key :: lst) fundec_map [] in
        let fundecs = rank_fn (List.map (fun fundec -> (FundecMap.find fundec file_of_fundec, fundec)) fundecs) in
        let rec get = function
            | fundec :: tail -> (
                let queue = FundecMap.find fundec fundec_map in
                match queue#get with
                | Some (queue, job) ->
                    let fundec_map = FundecMap.add fundec queue fundec_map in
                    Some ({< fundec_map = fundec_map >}, job)
                | None -> get tail
                )
            | [] -> None
        in
        get fundecs
        end
end

