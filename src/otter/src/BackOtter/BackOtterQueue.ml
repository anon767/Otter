open OcamlUtilities
open OtterCore

type queues = [
    | `B_InterSDSE
    | `B_InterSDSE_rr
    | `B_InterSDSE_pr
    | `B_InterSDSE_eff
    | `B_InterSDSE_rr_eff
    | `B_IntraSDSE
    | OtterQueue.Queue.queues
    | `B_Phases of (queues * (unit -> bool)) list
    | `B_RoundRobin of queues list
    | `B_Batched of queues
]

let exp_decay exp =
    let timenow () = BackOtterTimer.get_time_now () in
    let get_count () = !BackOtterTargetTracker.remove_count in
    let timestart = timenow () in
    let count = ref (get_count()) in
    let duration = ref Pervasives.infinity in
    fun () ->
        let cur_count = get_count () in
        let time = timenow () -. timestart in
        if time > !duration then false else begin
            if cur_count > !count then begin
                count := cur_count;
                duration := exp *. time
            end;
            true
        end

let queues : (string * queues) list = [
    "IntraSDSE"                                                      , `B_IntraSDSE;
    "InterSDSE"                                                      , `B_InterSDSE;
    "InterSDSE-roundrobin"                                           , `B_InterSDSE_rr;
    "InterSDSE-probabilistic"                                        , `B_InterSDSE_pr;
    "InterSDSE-efficient"                                            , `B_InterSDSE_eff;
    "InterSDSE-roundrobin-efficient"                                 , `B_InterSDSE_rr_eff;
    "RoundRobin(RandomPath,InterSDSE)"                               , `B_RoundRobin [`B_InterSDSE;`PathWeighted];
    "RoundRobin(RandomPath,InterSDSE-efficient)"                     , `B_RoundRobin [`B_InterSDSE_eff;`PathWeighted];
    "Batched(InterSDSE)"                                             , `B_Batched `B_InterSDSE;
    "Batched(InterSDSE-roundrobin)"                                  , `B_Batched `B_InterSDSE_rr;
    "Batched(InterSDSE-probabilistic)"                               , `B_Batched `B_InterSDSE_pr;
    "Batched(InterSDSE-efficient)"                                   , `B_Batched `B_InterSDSE_eff;
    "Batched(InterSDSE-roundrobin-efficient)"                        , `B_Batched `B_InterSDSE_rr_eff;
    "Batched(RoundRobin(RandomPath,InterSDSE))"                      , `B_Batched (`B_RoundRobin [`B_InterSDSE;`PathWeighted]);
    "Batched(RoundRobin(RandomPath,InterSDSE-efficient))"            , `B_Batched (`B_RoundRobin [`B_InterSDSE_eff;`PathWeighted]);
    "Batched(RoundRobin(RandomPath,InterSDSE-roundrobin))"           , `B_Batched (`B_RoundRobin [`B_InterSDSE_rr;`PathWeighted]);
    "Batched(RoundRobin(RandomPath,InterSDSE-roundrobin-efficient))" , `B_Batched (`B_RoundRobin [`B_InterSDSE_rr_eff;`PathWeighted]);
    "Batched(Phases(KLEE,InterSDSE))"                                , `B_Batched (`B_Phases [(`KLEE,exp_decay 3.);(`B_InterSDSE_eff, OtterQueue.PhasesQueue.immortal)]);
    "Batched(Phases(KLEE,InterSDSE-efficient))"                      , `B_Batched (`B_Phases [(`KLEE,exp_decay 3.);(`B_InterSDSE_eff, OtterQueue.PhasesQueue.immortal)]);
    "Batched(Phases(KLEE,InterSDSE-roundrobin))"                     , `B_Batched (`B_Phases [(`KLEE,exp_decay 3.);(`B_InterSDSE_rr, OtterQueue.PhasesQueue.immortal)]);
] @ (OtterQueue.Queue.queues :> (string * queues) list)

let rec get file = function
    | `B_IntraSDSE                          -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t ~interprocedural:false OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `B_InterSDSE                          -> new OtterQueue.RankedQueue.t [ new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `B_InterSDSE_rr                       -> new OtterQueue.RankedQueue.t [ new RoundRobinClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional]
    | `B_InterSDSE_pr                       -> new OtterQueue.RankedQueue.t [ new OtterQueue.WeightedRandomStrategy.t (new ClosestToTargetsStrategy.t OtterQueue.ClosestToTargetsStrategy.inversely_proportional) ]
    | `B_InterSDSE_eff                      -> new OtterQueue.PrioritizerRandomQueue.t (new OtterQueue.PriorityQueuePrioritizer.t ~need_update:(fun () ->
                                                        if !BackOtterTargetTracker.update_flag then begin
                                                            BackOtterTargetTracker.update_flag := false;
                                                            true
                                                        end else false) (ClosestToTargetsStrategy.weight OtterQueue.ClosestToTargetsStrategy.inversely_proportional))
    | `B_InterSDSE_rr_eff                   ->
            new OtterQueue.PrioritizerRandomQueue.t (
                new OtterQueue.RoundRobinPrioritizer.t (
                    List.map (fun line_target -> new OtterQueue.PriorityQueuePrioritizer.t (
                        SDSEUtilities.interSDSE_score [line_target]),
                        fun () -> List.memq line_target (BackOtterTargetTracker.get_line_targets file)
                    ) (BackOtterTargetTracker.get_line_targets file)
            ))
    | `B_Batched q                          -> new OtterQueue.BatchQueue.t (get file (q :> queues))
    | `B_RoundRobin qs                      -> new OtterQueue.RoundRobinQueue.t (List.map (fun q -> get file (q :> queues)) qs)
    | `B_Phases qps                         -> new OtterQueue.PhasesQueue.t (List.map (fun (q,p) -> (get file (q :> queues),p)) qps)
    | #OtterQueue.Queue.queues as queue     -> OtterQueue.Queue.get queue

(* Forward *)
let default_fqueue = ref (`Generational `BreadthFirst)
let get_default_fqueue file = get file !default_fqueue

(* Backward *)
let get_function_backward_queue file function_queue queue =
    new FunctionQueue.t (FunctionRanker.get function_queue) (fun () -> get file queue)

let default_bqueue = ref `RandomPath
let get_default_bqueue file = get_function_backward_queue file !FunctionRanker.default_function_rank !default_bqueue

let options = [
    "--forward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_fqueue := List.assoc name queues),
        " Set the default forward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_fqueue) queues)) ^ ")";
    "--backward-queue",
        Arg.Symbol (fst (List.split queues), fun name -> default_bqueue := List.assoc name queues),
        " Set the default backward job queue in bi-directional BackOtter (default: " ^ (fst (List.find (fun (_, x) -> x = !default_bqueue) queues)) ^ ")";
]

