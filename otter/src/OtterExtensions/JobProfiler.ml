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

    type t = (LocationSet.t * float) CallstackMap.t
    let empty = CallstackMap.empty

    let update callstack loc time tree = Profiler.global#call "LocationTree.update" begin fun () ->
        let rec update path locopt tree =
            let locs, time' = if CallstackMap.mem path tree then CallstackMap.find path tree else LocationSet.empty, 0. in
            let time' = time +. time' in
            let locs = match locopt with None -> locs | Some loc -> LocationSet.add loc locs in
            let tree = CallstackMap.add path (locs, time') tree in
            match path with [] -> tree | loc :: path -> update path (Some loc) tree
        in
        update (loc::callstack) None tree
    end

    let fold ff tree acc = CallstackMap.fold (fun path (_, time) acc -> match path with loc :: _ -> ff loc time acc | [] -> acc) tree acc
end

module Profile = struct

    type t = {
        tree : LocationTree.t;
    }
    let empty = {
        tree = LocationTree.empty;
    }

    let update profile job time = {
        tree = LocationTree.update job#caller_list (Job.get_loc job) time profile.tree;
    }

    let flush profile = 
        LocationTree.fold (fun loc time _ -> Format.printf "Location %a takes %.2f seconds@\n" Printcil.loc loc time) profile.tree ()
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
