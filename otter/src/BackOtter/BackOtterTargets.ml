open OtterCore
open OtterCFG

module FundecMap = Map.Make (CilUtilities.CilData.CilFundec)
module InstructionMap = Map.Make (Instruction)

module PathSet = Set.Make (DecisionPath)

(* TODO: generalize targets to program points, not just functions *)
type t = {
    fundec_to_pathset : PathSet.t FundecMap.t;
    instruction_to_pathset: PathSet.t InstructionMap.t;
    last_failing_path : (Cil.fundec * DecisionPath.t) option;
}

let empty = {
    fundec_to_pathset = FundecMap.empty;
    instruction_to_pathset = InstructionMap.empty;
    last_failing_path = None;
}

let targets_ref = ref empty

let reset_targets () = targets_ref := empty

let get_pathset fundec =
    let targets = !targets_ref in
    if FundecMap.mem fundec targets.fundec_to_pathset then
        FundecMap.find fundec targets.fundec_to_pathset
    else PathSet.empty

let get_paths fundec = 
    PathSet.elements (get_pathset fundec)

let get_pathset_of_instruction instruction =
    try InstructionMap.find instruction (!targets_ref).instruction_to_pathset with Not_found -> PathSet.empty

(**
 *  @return true if [path] is a new path
 *)
let add_path fundec path instruction_opt =
    let targets = !targets_ref in
    let failing_paths = get_pathset fundec in
    if PathSet.mem path failing_paths then false
    else
        let _ = targets_ref :=
        {
            fundec_to_pathset = FundecMap.add fundec (PathSet.add path failing_paths) targets.fundec_to_pathset;
            instruction_to_pathset = begin match instruction_opt with
                    | Some instruction -> InstructionMap.add instruction (get_pathset_of_instruction instruction) targets.instruction_to_pathset
                    | None -> targets.instruction_to_pathset
                end;
            last_failing_path = Some (fundec, path);
        } in
        true

let get_last_failing_path () =
    let targets = !targets_ref in
    targets_ref := { targets with last_failing_path = None };
    targets.last_failing_path

let is_target fundec = 
    get_paths fundec <> []

let get_target_fundecs () =
    let targets = !targets_ref in
    FundecMap.fold (
        fun target_fundec _ target_fundecs -> target_fundec :: target_fundecs
    ) targets.fundec_to_pathset []

let remove_target_instruction instruction =
    let pathset = get_pathset_of_instruction instruction in
    (* Remove failing paths associated with this target, except those in main. *)
    let fundec_to_pathset = FundecMap.mapi (fun fundec old_pathset -> 
        if CilUtilities.CilData.CilFundec.equal fundec (ProgramPoints.get_entry_fundec instruction.Instruction.file) then
            old_pathset 
        else
            PathSet.diff old_pathset pathset
        ) (!targets_ref).fundec_to_pathset in
    (* Remove function targets that have no more failing paths *)
    let fundec_to_pathset = FundecMap.filter (fun _ new_pathset -> not (PathSet.is_empty new_pathset)) fundec_to_pathset in
    (* Remove binding of instruction *)
    let instruction_to_pathset = InstructionMap.remove instruction (!targets_ref).instruction_to_pathset in
    (* last_failing_path should be from main, therefore can be ignored. *)
    targets_ref := { (!targets_ref) with fundec_to_pathset = fundec_to_pathset; instruction_to_pathset = instruction_to_pathset; }

