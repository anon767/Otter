open OtterCore

let external_bounding_path = ref ExternalBoundingPath.empty

class t :
    object ('self)
        (** Bounding paths
         *  None: this job is NOT a bounded job
         *  Some []: this is a bounded job, and has fallen out of bound
         *  Some paths: this is a bounded job, and is still bounded
         *)
        method bounding_paths : DecisionPath.t list option
        method with_bounding_paths : DecisionPath.t list option -> 'self

        (** External bounding paths: similar to bounding paths, but given as BackOtter's input. Used by BoundingPathsStrategy.
         *  The boolean is to mark if the external bounding path is activated or not. It is activated if the job's latest
         *  decision matches the first decision in the external bounding path.
         *)
        method external_bounding_paths : ExternalBoundingPath.t list option
        method with_external_bounding_paths : ExternalBoundingPath.t list option -> 'self

        (** Some (varinfo) if the job is at the beginning of a function call. 
          * BidirectionalQueue.t#put only verifies jobs over paths-to-targets when jobs correspond to beginning of function calls.
          * (Checking if the head of decision_path is not enough, because the function may begin with some straightline code that does not generate new decisions.)
          *)
        method latest_function_call: Cil.varinfo option 
        method clear_latest_function_call: 'self

        (** This method updates bounding_paths and latest_function_call given the decision.
          * Used by BackOtterFileJob and BackOtterFunctionJob to override append_decision_path.
          *)
        method postprocess_append_decision_path : Decision.t -> 'self

        method become : 'self -> unit
    end
=
    object (self : 'self)
        val mutable bounding_paths = None
        method bounding_paths = bounding_paths
        method with_bounding_paths bounding_paths = {< bounding_paths = bounding_paths >}

        val mutable external_bounding_paths = Some [!external_bounding_path]
        method external_bounding_paths = external_bounding_paths
        method with_external_bounding_paths external_bounding_paths = {< external_bounding_paths = external_bounding_paths >}

        val mutable latest_function_call = None
        method latest_function_call = latest_function_call
        method clear_latest_function_call = {< latest_function_call = None >}

        method postprocess_append_decision_path decision = 
            let bounding_paths = match bounding_paths with
            | None -> None
            | Some paths -> 
                Some (List.fold_left (
                    fun paths path ->
                        if DecisionPath.length path > 0 && Decision.equal (DecisionPath.hd path) decision then
                            (DecisionPath.tl path) :: paths
                        else paths
                ) [] paths)
            in
            let external_bounding_paths = match external_bounding_paths with
            | None -> None
            | Some paths -> 
                Some (List.fold_left (
                    fun paths path ->
                        if ExternalBoundingPath.length path > 0 && (ExternalBoundingPath.match_hd path (OcamlUtilities.FormatPlus.as_string Decision.print decision)) then
                            (ExternalBoundingPath.tl path) :: paths
                        else if not (ExternalBoundingPath.activated path) then
                            path :: paths
                        else
                            paths
                ) [] paths)
            in
            let latest_function_call = match decision with
                | Decision.DecisionFuncall (_, varinfo) -> Some varinfo
                | _ -> None
            in
            {< bounding_paths = bounding_paths; 
               external_bounding_paths = external_bounding_paths; 
               latest_function_call = latest_function_call >}

        method become (other : 'self) =
            bounding_paths <- other#bounding_paths;
            external_bounding_paths <- other#external_bounding_paths;
            latest_function_call <- other#latest_function_call

    end

let options = [
    ("--external-bounding-paths",
        Arg.String begin fun filename ->
            let inChan = open_in filename in
            let n = in_channel_length inChan in
            let buffer = String.create n in
            really_input inChan buffer 0 n;
            external_bounding_path := ExternalBoundingPath.create buffer;
            close_in inChan
        end,
        "<filename> File containing lines in the external bounding path.\n");
]
