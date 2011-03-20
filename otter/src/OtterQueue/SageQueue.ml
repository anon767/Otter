(** SAGE job queue, based on "Automated whitebox fuzz testing".

    High-level view of this scheme:
    1. Conceptually, there are two modes: execution mode (EXE) and find-next mode (NEXT).
    2. In NEXT mode, the queue contains some completed but unpicked paths.
       Each path (parent) has a list of branching points below the branch where the parent was born.
       Each path has a score which is the extra blocks the path covered when it finished. This score is unchanged once set.
       The goal in NEXT mode is to find the next path to expand. 
       The path with the highest score is "picked", and its list of branching points will be processed. The mode is switched to EXE.
    3. In EXE mode, a list of branching points is being processed.
       Each branching point is run till completion. During the execution of a path, its list of branching points is collected as well.
       When there's no more execution to run, the mode is switched to NEXT.
       
    Implementation notes:
    1. This queue cannot be combined with others, and therefore #remove is not supported.
    2. The NEXT mode is solely in #get, while #put is always in EXE mode.

    Notice: this is potentially more efficient than real SAGE, because real SAGE runs programs natively and has to repeat work. 
    (But on the other hand native execution is cheap.)

    P. Godefroid, M. Y. Levin, and D. Molnar. Automated whitebox fuzz testing. In Proceedings of NDSS '08 (Network and
    Distributed Systems Security), pages 151--166, 2008.

    @see <https://www.isoc.org/isoc/conferences/ndss/08/papers/10_automated_whitebox_fuzz.pdf>
    https://www.isoc.org/isoc/conferences/ndss/08/papers/10_automated_whitebox_fuzz.pdf
 *)

open DataStructures
open OtterCore

module Path = struct
    type t = {
        path_id : int;
        branching_points : Job.t list;
        score : int;
    }
    let compare a b = 
        match Pervasives.compare a.score b.score with
        | 0 -> Pervasives.compare a.path_id b.path_id
        | i -> i
end

module PathSet = Set.Make(Path)
module StmtInfoSet = Job.StmtInfoSet

class ['self] t = object (self : 'self)

    val completed_paths = PathSet.empty
    val global_coveredBlocks = StmtInfoSet.empty
    val branching_points = []
    val cur_parent = None
    val cur_children = RandomBag.empty
    val cur_branching_points = []  (* TODO: make this RandomBag, although real SAGE doesn't specify how this is done *)


    (* Enable block coverage *)
    initializer Executeargs.arg_block_coverage := true

    method put job = 
        begin match cur_parent with
            | None -> (* Very beginning *) ()
            | Some parent -> assert(job#path_id = parent#path_id || job#parent_path_id = parent#path_id)
        end;
        {< cur_children = RandomBag.put job cur_children; >}


    method remove job = 
        failwith "#remove does not apply to SageQueue"

    method get = 
        match RandomBag.get cur_children with
        | None -> (* The path ended with cur_parent is completed *)
            let completed_paths, global_coveredBlocks = 
                if cur_branching_points = [] then (* The completed path does not branch, therefore we don't look at it anymore *)
                    completed_paths, global_coveredBlocks
                else
                    let parent = 
                        match cur_parent with
                        | Some parent -> parent
                        | None -> failwith "Parent is None"
                    in
                    let score, global_coveredBlocks = 
                        (* Alert: must enable --blockCov *)
                        let coveredBlocks = parent#exHist.Job.coveredBlocks in
                        let score = StmtInfoSet.cardinal (StmtInfoSet.diff coveredBlocks global_coveredBlocks) in
                        let global_coveredBlocks = StmtInfoSet.union global_coveredBlocks coveredBlocks in
                        score, global_coveredBlocks
                    in
                    let path = {
                        Path.path_id = parent#path_id;
                        Path.branching_points = cur_branching_points;
                        Path.score = score;
                    } in
                    PathSet.add path completed_paths, global_coveredBlocks
            in
            begin match branching_points with
                | branching_point :: branching_points -> 
                    Some({< completed_paths = completed_paths;
                            global_coveredBlocks = global_coveredBlocks;
                            branching_points =  branching_points;
                            cur_parent = Some branching_point;
                            cur_children = RandomBag.empty;
                            cur_branching_points = [];
                          >}, branching_point)
                | [] -> (* NEXT mode *)
                    if PathSet.is_empty completed_paths then None
                    else
                        let chosen_path = PathSet.max_elt completed_paths in
                        let completed_paths = PathSet.remove chosen_path completed_paths in (* Jobs are removed from memory at this point *)
                        let branching_points = chosen_path.Path.branching_points in
                        (* assert(branching_points <> []) *)
                        let parent = List.hd branching_points in
                        Some({< completed_paths = completed_paths; 
                                global_coveredBlocks = global_coveredBlocks;
                                branching_points = List.tl branching_points; 
                                cur_parent = Some parent;
                                cur_children = RandomBag.empty;
                                cur_branching_points = [];
                              >}, parent)
            end
        | Some (rest, chosen_child) ->
            Some({< cur_parent = Some chosen_child;
                    cur_children = RandomBag.empty;
                    cur_branching_points = RandomBag.fold (fun cur_branching_points job -> job :: cur_branching_points) cur_branching_points rest;
                  >}, chosen_child)

end

