open OcamlUtilities
open OtterCore

module type KeyType = sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    val of_job : ((_,_) #Job.t -> t)
end

module LocationTree = struct
    module Location = CilUtilities.CilData.CilLocation
    module LocationSet = Set.Make(Location)
    module CallstackMap = Map.Make(struct type t = Location.t list let compare = ListPlus.compare Location.compare end)
    module type RecordType = sig 
        type t 
        val zero : t 
        val merge : t -> t -> t 
        val merge_caller : t -> t -> t 
    end

    module Make (Record : RecordType) = struct
        type t = (LocationSet.t * Record.t) CallstackMap.t
        let empty = CallstackMap.empty

        let update callstack loc record tree = Profiler.global#call "LocationTree.update" begin fun () ->
            let tree =
                let locs, record' = if CallstackMap.mem callstack tree then CallstackMap.find callstack tree else LocationSet.empty, Record.zero in
                let record' = Record.merge record record' in
                CallstackMap.add (loc::callstack) (locs, record') tree
            in
            let rec update callstack loc tree =
                let locs, record' = if CallstackMap.mem callstack tree then CallstackMap.find callstack tree else LocationSet.empty, Record.zero in
                let record' = Record.merge_caller record record' in
                let locs = LocationSet.add loc locs in
                let tree = CallstackMap.add callstack (locs, record') tree in
                match callstack with [] -> tree | loc :: callstack -> update callstack loc tree
            in
            update callstack loc tree
        end

        let fold ff tree acc = CallstackMap.fold (fun callstack (_, record) acc -> match callstack with loc :: _ -> ff loc record acc | [] -> acc) tree acc
    end
end

module Profile = struct

    module IntSet = Set.Make (struct type t = int let compare = Pervasives.compare end)
    module Record = struct 
        type t = {
            time : float;
            nodes : IntSet.t;
        }
        let zero = {
            time = 0.;
            nodes = IntSet.empty;
        }
        let merge r1 r2 = {
            time = r1.time +. r2.time;
            nodes = IntSet.union r1.nodes r2.nodes;
        }
        let merge_caller r_new r_old = {
            time = r_new.time +. r_old.time;
            nodes = r_old.nodes;
        }
    end
    module LocationTree = LocationTree.Make (Record)

    type t = {
        tree : LocationTree.t;
    }
    let empty = {
        tree = LocationTree.empty;
    }

    let update profile job time = 
        let record = {
            Record.time = time;
            Record.nodes = IntSet.singleton job#node_id;
        } in {
            tree = LocationTree.update job#caller_list (Job.get_loc job) record profile.tree; 
        }

    let flush profile = 
        LocationTree.fold (fun loc record _ -> Format.printf "Location %a takes %.2f seconds, visited by %d paths.@\n" Printcil.loc loc record.Record.time (IntSet.cardinal record.Record.nodes)) profile.tree ()
end

module Make (Key : KeyType) = struct

    module Hashtbl = Hashtbl.Make (Key)
    let hashtbl : Profile.t Hashtbl.t = Hashtbl.create 8
    let last_profile_update = ref None

    let interceptor job interceptor = 
        Profiler.global#call "JobProfiler.interceptor" begin fun () ->
            let time = Unix.gettimeofday () in
            begin match !last_profile_update with
            | None -> ()
            | Some (key, profile, time') ->
                let profile = profile (time -. time') in
                Hashtbl.replace hashtbl key profile
            end;
            (* This job will be processed by next call to this interceptor *)
            let key = Key.of_job job in
            let profile = try Hashtbl.find hashtbl key with Not_found -> Profile.empty in
            let profile = Profile.update profile job in
            last_profile_update := Some (key, profile, time)
        end;
        interceptor job

    let flush () = Hashtbl.iter (fun _ -> Profile.flush) hashtbl
end
