open OtterCFG
open Cil

module RD = MyReachingDefs

let get_lrhs instruction =  
    match instruction.Instruction.stmt.skind with
    | Cil.If (exp, _, _, _) -> None, Some exp
    | Cil.Instr _ -> 
        begin match instruction.Instruction.instrs with
            | Cil.Set (lval, exp, _) :: _ -> Some lval, Some exp
            | _ -> None, None
        end
    | _ -> None, None


(**
 *  TODO:
 *  Here, a definition is an assignment instruction. rhs is the right-hand-side of the definition, 
 *  which must be a Cil.exp.
 *
 *  Given a definition, compute a mapping from values to sets of lists of definitions, 
 *  each of which must lead to the value.
 *
 *  Algorithm: given a definition, 
 *  1. Find the varinfos involved in the rhs.
 *  2. For each varinfo, find its reaching definitions.
 *  3. For each reaching definition, recursively find the mapping.
 *  4. Rearrange data found in (2) and (3), to get a mapping (for each varinfo) which maps possible values to the list(s) of definitions,
 *  5. For each combination of values that varinfos can take (i.e., for each element in the Cartesian product {values of varinfo x} X {values of varinfo y} X ...), evaluate the rhs with the combination, and merge all the associated sets of reaching definitions to be the final one.
 *)
let find instruction =
    let varinfo_map = RD.getRDs instruction in
    let lhs, rhs = get_lrhs instruction in
    match rhs with
    | Some (Lval(Var(varinfo),NoOffset)) ->
        let (instruction_set, _) = RD.VarinfoMap.find varinfo varinfo_map in
        begin try Some (RD.InstructionSet.choose instruction_set) with Not_found -> None end
    | _ -> None

