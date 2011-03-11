open OcamlUtilities

module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)

class ['self] t rank_fn queue_constructor = object (self : 'self)
    val fundec_map = FundecMap.empty

    method put job =
        Profiler.global#call "FunctionQueue.t#put" begin fun () ->
            let origin_function = BackOtterUtilities.get_origin_function job in
            let file, queue, count =
                try
                    FundecMap.find origin_function fundec_map 
                with Not_found -> job#file, queue_constructor (), 0
            in
            let fundec_map =
                let queue = queue#put job in
                FundecMap.add origin_function (file, queue, count + 1) fundec_map
            in
            {< fundec_map = fundec_map; >}
        end

    method remove job =
        Profiler.global#call "FunctionQueue.t#remove" begin fun () ->
            try
                let origin_function = BackOtterUtilities.get_origin_function job in
                let file, queue, count = FundecMap.find origin_function fundec_map in
                let queue = queue#remove job in
                let fundec_map = FundecMap.add origin_function (file, queue, count - 1) fundec_map in
                {< fundec_map = fundec_map >}
            with Not_found ->
                self
        end        

    method get =
        Profiler.global#call "FunctionQueue.t#get" begin fun () ->
            let lst = FundecMap.fold (fun fundec (file, _, count) lst -> if count > 0 then (file, fundec) :: lst else lst) fundec_map [] in
            if lst = [] then None
            else
                let fundec = List.hd (rank_fn lst) in
                let file, queue, count = FundecMap.find fundec fundec_map in
                match queue#get with
                | Some (queue, job) ->
                    let fundec_map = FundecMap.add fundec (file, queue, count - 1) fundec_map in
                    Some ({< fundec_map = fundec_map >}, job)
                | None -> assert(false)
        end
end

