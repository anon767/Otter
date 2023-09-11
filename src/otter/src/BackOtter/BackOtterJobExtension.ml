open OtterCore

class t :
    object ('self)
        (** A job created by new may be uninitialized. In that case, invoke [initialize] to initialize. *)
        method initialize : 'self
        method is_initialized : bool

        (** Bounding paths
         *  None: this job is NOT a bounded job
         *  Some []: this is a bounded job, and has fallen out of bound
         *  Some paths: this is a bounded job, and is still bounded
         *)
        method bounding_paths : DecisionPath.t list option
        method with_bounding_paths : DecisionPath.t list option -> 'self

        (** Some (varinfo) if the job is at the beginning of a function call. 
          * BidirectionalQueue.t#put only verifies jobs over paths-to-targets when jobs correspond to beginning of function calls.
          * (Checking if the head of decision_path is not enough, because the function may begin with some straightline code that does not generate new decisions.)
          *)
        method last_function_call: Cil.varinfo option 
        method clear_last_function_call: 'self

        (** Enable/disable recording decisions *)
        method enable_record_decisions: bool
        method with_enable_record_decisions : bool -> 'self

        (** This method updates bounding_paths and last_function_call given the decision.
          * Used by BackOtterFileJob and BackOtterFunctionJob to override append_decision_path.
          *)
        method postprocess_append_decision_path : Decision.t -> 'self

        method printer : Format.formatter -> unit

        method become : 'self -> unit
    end
=
    object (self : 'self)
        val mutable is_initialized = false
        method initialize = {< is_initialized = true >}
        method is_initialized = is_initialized

        val mutable enable_record_decisions = true
        method enable_record_decisions = enable_record_decisions
        method with_enable_record_decisions enable_record_decisions = {< enable_record_decisions = enable_record_decisions >}

        val mutable bounding_paths = None
        method bounding_paths = bounding_paths
        method with_bounding_paths bounding_paths = {< bounding_paths = bounding_paths >}

        val mutable last_function_call = None
        method last_function_call = last_function_call
        method clear_last_function_call = {< last_function_call = None >}

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
            let last_function_call = match decision with
                | Decision.DecisionFuncall (_,_, varinfo) -> Some varinfo
                | _ -> None
            in
            {< bounding_paths = bounding_paths; 
               last_function_call = last_function_call >}

        method printer ff =
            let module F = OcamlUtilities.FormatPlus in
            let module C = CilUtilities.CilPrinter in
            Format.fprintf ff "BackOtterJobExtension@;";
            Format.fprintf ff "enable_record_decisions: %B@;" enable_record_decisions;
            Format.fprintf ff "last_function_call: @[<hov>%a@]@;" (F.option_printer C.varinfo) last_function_call;
            Format.fprintf ff "bounding_paths: @[<hov>%a@]@;" (F.option_printer (F.pp_print_list DecisionPath.print "@;")) bounding_paths


        method become (other : 'self) =
            is_initialized <- other#is_initialized;
            enable_record_decisions <- other#enable_record_decisions;
            bounding_paths <- other#bounding_paths;
            last_function_call <- other#last_function_call

    end

(* These functions, when invoked, will first run their built-in versions, and retract to C versions when fail. *)
let disabled_record_decisions_fname = ["memcpy"; "memmove"; "memset"]

let enable_record_decisions job =
    let current_fundec = List.hd job#state.State.callstack in
    (not (List.mem current_fundec.Cil.svar.Cil.vname disabled_record_decisions_fname)) && job#enable_record_decisions
