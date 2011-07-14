open OtterCore

module IdMap = Map.Make(struct type t = int let compare = Pervasives.compare end) 
module LocMap = Map.Make(CilUtilities.CilData.CilLocation)

let idmap = ref IdMap.empty
let locmap = ref LocMap.empty

let get_entry loc = try LocMap.find loc (!locmap) with Not_found -> let new_entry = 0.0 in locmap := LocMap.add loc new_entry (!locmap); new_entry
let add_time loc t = let t' = get_entry loc in locmap := LocMap.add loc (t+.t') (!locmap)

let time job fn =
    let ret = Stats.timethis fn () in
    let time_elapsed = (!Stats.lastTime) in
    let id = job#instr_id in
    let caller_list = job#caller_list in
    let involved_list = if caller_list = [] then [id] else if List.hd caller_list = id then caller_list else id :: caller_list in
    let loc = Job.get_loc job in
    let _ = try assert(CilUtilities.CilData.CilLocation.compare (IdMap.find id (!idmap)) loc == 0) with Not_found -> idmap := IdMap.add id loc (!idmap) in
    List.iter (fun loc -> add_time loc time_elapsed) (List.map (fun id -> try IdMap.find id (!idmap) with Not_found -> Cil.locUnknown) involved_list);
    ret

let flush () =
    LocMap.iter (fun loc t -> Format.printf "Location %a takes %.2f seconds @\n" Printcil.loc loc t) (!locmap)
