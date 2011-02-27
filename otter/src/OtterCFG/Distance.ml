(** Find distances from instructions to function returns.
 *  This implementation uses Dijkstra's algorithm from Ocamlgraph.
 *)
open Ocamlgraph
open CilUtilities

(**/**) (* various helpers *)
module CilFileFundec = CilData.WithFile(CilData.CilFundec)

(** Find all the return sites in this function. *)
let return_sites =
    let module Memo = OcamlUtilities.Memo.Make (CilFileFundec) in
    Memo.memo "Distance.return_sites" (fun (file, fundec) ->
        let return_sites = ref [] in
        ignore (Cil.visitCilFunction (object
            inherit Cil.nopCilVisitor
            method vstmt stmt = match stmt.Cil.skind with
                | Cil.Return _ ->
                    return_sites := (Instruction.of_stmt_first file fundec stmt)::!return_sites;
                    Cil.SkipChildren
                | _ ->
                    Cil.DoChildren
        end) fundec);
        !return_sites
    ) 
(**/**)

(* G = (V, E) is a reversed CFG *)

(* A vertex is either an instruction, or an "imaginary" instruction that links to all return instructions of a function *)
module V = struct
    type t = VInstr of Instruction.t | VRet of CilFileFundec.t
    let compare v1 v2 = 
        match v1,v2 with
        | VInstr i1, VInstr i2 -> Instruction.compare i1 i2
        | VRet f1, VRet f2 -> CilFileFundec.compare f1 f2
        | VInstr _, VRet _ -> 1
        | VRet _, VInstr _ -> -1
    let hash = function VInstr i -> Instruction.hash i | VRet f -> CilFileFundec.hash f
    let equal v1 v2 = compare v1 v2 = 0
    let printer ff = function
        | VInstr instr -> Format.fprintf ff "VInstr(%a)" Instruction.printer instr
        | VRet (file, fundec) -> Format.fprintf ff "VRet(%s)" fundec.Cil.svar.Cil.vname
end 

(* Directed edge specified by the tail node. Edges going out from the head node are specified by G.iter_succ_e.  *)
module E = struct
    type t = V.t 
    type label = V.t
    let label edge = edge
    let dst edge = edge
    let printer ff edge = Format.fprintf ff "->%a" V.printer edge
end 

module G = struct
    type t = unit
    module V = V
    module E = E
    let iter_succ_e fn g v = 
        let preds = match v with
            | V.VInstr i -> Instruction.predecessors i 
            | V.VRet f -> return_sites f
        in List.iter fn (List.map (fun i -> V.VInstr i) preds)
end 
        
module VPairHash = Hashtbl.Make (struct
    type t = V.t * V.t
    let equal (x1,x2) (y1,y2) = (V.equal x1 y1) && (V.equal x2 y2)
    let hash (x1,x2) = Hashtbl.hash (V.hash x1, V.hash x2)
end) 

(* Non-transitive; assuming src and des are from the same function. *)
let rec find_impl =

    let distance_hash = VPairHash.create 0 in

    fun visited src des ->
        try
            VPairHash.find distance_hash (src, des)

        with Not_found -> 

            (* Weight of edges *)
            let module W = struct
                type label = V.t
                type t = int
                let weight = function
                    (* label is the pred instr of this instr.
                     * If label is not a function call, then weight = 1
                     * else, lookup the weight of the function call.
                     *)
                    | V.VInstr i -> 
                        let call_targets = Instruction.call_targets i in
                        if call_targets = [] then 1
                        else
                            let call_target_distances = List.map (fun call_target ->
                                let caller = call_target.Instruction.fundec in
                                if List.memq caller visited then max_int (* Approcimate; don't bother doing fixpoint *)
                                else find_impl (caller :: visited) (V.VRet (call_target.Instruction.file, call_target.Instruction.fundec)) (V.VInstr call_target) + 1  
                            ) call_targets in
                            List.fold_left min max_int call_target_distances
                    | V.VRet (file, fundec) -> assert(false) (* There won't be edges going into a VRet *)
                let compare = Pervasives.compare
                let add x y = let s = x + y in if s < 0 then max_int else s (* avoid overflow *)
                let zero = 0
            end in
            
            let module Dij = Path.Dijkstra (G) (W) in

            OcamlUtilities.Profiler.global#call "Distance.find_impl (uncached)" begin fun () ->

                let _, dist = 
                    try Dij.shortest_path () src des
                    with Not_found -> [], max_int
                in
                VPairHash.replace distance_hash (src, des) dist;
                dist
            end

let find_return instr = 
    let vret = V.VRet (instr.Instruction.file, instr.Instruction.fundec) in
    let vinstr = V.VInstr instr in
    find_impl [] vret vinstr

let find instr targets = 
    let vtargets = List.map (fun target -> V.VInstr target) targets in
    let vinstr = V.VInstr instr in
    let distances = List.map (fun vtarget -> find_impl [] vtarget vinstr) vtargets in
    List.fold_left min max_int distances

let find_in_context instr context targets =
    (* compute the distance from the instr through function returns to targets in the call context *)
    let rec unwind dist return_dist = function
        | call_return::context ->
            let dist =
                (* "+1" since call_return is the NEXT instruction after the call *)
                let dist' = return_dist + 1 + find call_return targets in
                if dist' < 0 then dist (* overflow *) else min dist dist'
            in
            (* "+1" since call_return is the NEXT instruction after the call *)
            let return_dist = return_dist + 1 + find_return call_return in
            if return_dist < 0 then
                dist (* overflow; terminate since further unwindings will also overflow *)
            else
                unwind dist return_dist context
        | [] ->
            dist
    in
    (* compute the initial distance to targets and distance to function returns *)
    let dist = find instr targets in
    let return_dist = find_return instr in
    unwind dist return_dist context

