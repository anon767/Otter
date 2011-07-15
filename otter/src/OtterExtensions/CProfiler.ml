open OtterCore

module IdMap = Map.Make(struct type t = int let compare = Pervasives.compare end) 

let idmap = ref IdMap.empty

module LocMap = struct
    module LocMap = Map.Make(CilUtilities.CilData.CilLocation)
    let locmap = ref LocMap.empty
    let get_entry loc = try LocMap.find loc (!locmap) with Not_found -> let new_entry = 0.0 in locmap := LocMap.add loc new_entry (!locmap); new_entry
    let add_time loc t = let t' = get_entry loc in locmap := LocMap.add loc (t+.t') (!locmap)
    let flush () = LocMap.iter (fun loc t -> Format.printf "Location %a takes %.2f seconds @\n" Printcil.loc loc t) (!locmap)
end

module FundecMap = struct
    module FundecMap = Map.Make(CilUtilities.CilData.CilFundec)
    let funmap = ref FundecMap.empty
    let get_entry f = try FundecMap.find f (!funmap) with Not_found -> let new_entry = 0.0 in funmap := FundecMap.add f new_entry (!funmap); new_entry
    let add_time f t = let t' = get_entry f in funmap := FundecMap.add f (t+.t') (!funmap)
    let flush () = FundecMap.iter (fun f t -> Format.printf "Fundec %s takes %.2f seconds @\n" f.Cil.svar.Cil.vname t) (!funmap)
end

let time job fn =
    let ret = Stats.timethis fn () in
    let time_elapsed = (!Stats.lastTime) in
    let id = job#instr_id in
    let caller_list = job#caller_list in
    let involved_list = if caller_list = [] then [id] else if List.hd caller_list = id then caller_list else id :: caller_list in
    let loc = Job.get_loc job in
    let fundec =
        let instructions = OtterCFG.Instruction.by_line job#file (loc.Cil.file, loc.Cil.line) in
        match instructions with
        | [] -> Format.printf "Location %a does not correspond to any instruction!!!@\n" Printcil.loc loc; Cil.dummyFunDec
        | instruction :: _ -> instruction.OtterCFG.Instruction.fundec
    in
    idmap := IdMap.add id (loc, fundec) (!idmap);
    let involved_locs = List.map (fun id -> try fst (IdMap.find id (!idmap)) with Not_found -> Cil.locUnknown) involved_list in
    let involved_fundecs = List.map (fun id -> try snd (IdMap.find id (!idmap)) with Not_found -> Cil.dummyFunDec) involved_list in
    List.iter (fun loc -> LocMap.add_time loc time_elapsed) involved_locs;
    List.iter (fun f -> FundecMap.add_time f time_elapsed) involved_fundecs;
    ret

let flush () =
    LocMap.flush();
    Format.printf "--------------------------------------------------------@\n";
    FundecMap.flush()
