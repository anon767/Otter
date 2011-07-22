open OcamlUtilities
open CilUtilities
open CilUtilities.CilData
open Cil

module T : sig

    type t = private
        | DecisionConditional of Cil.location * Cil.stmt * bool (* if-statement *)
        | DecisionFuncall of Cil.location * Cil.instr * Cil.varinfo (* function call (including functions without bodies, e.g., builtins) *)
        (* TODO (martin): define DecisionLongjmp *)

    val make_Decision_Conditional : (Cil.stmt * bool) -> t
    val make_Decision_Funcall : (Cil.instr * Cil.varinfo) -> t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit

    val to_string : Format.formatter -> t -> unit
    val from_string : Cil.file -> string -> t

end = struct

    type t =
        | DecisionConditional of Cil.location * Cil.stmt * bool
        | DecisionFuncall of Cil.location * Cil.instr * Cil.varinfo

    let make_Decision_Conditional (stmt, truth) = DecisionConditional (Cil.get_stmtLoc stmt.skind, stmt, truth)
    let make_Decision_Funcall (instr, varinfo) = DecisionFuncall (Cil.get_instrLoc instr, instr, varinfo)

    let compare d1 d2 =
        let loc_compare loc1 loc2 = Tuple.combine_compares String.compare Pervasives.compare (loc1.file, loc1.line) (loc2.file, loc2.line) in  (* FIXME: omitted Cil.byte, which seems unstable over different runs *)
        match d1, d2 with
        | DecisionConditional (loc1, stmt1, bool1), DecisionConditional (loc2, stmt2, bool2) -> Tuple.combine_compares loc_compare Pervasives.compare (loc1, bool1) (loc2, bool2)
        | DecisionFuncall (loc1, instr1, varinfo1), DecisionFuncall (loc2, instr2, varinfo2) -> Tuple.combine_compares loc_compare Pervasives.compare (loc1, varinfo1) (loc2, varinfo2)
        | DecisionConditional _, _ -> 1
        | _, DecisionConditional _ -> -1

    let equal d1 d2 = compare d1 d2 = 0

    let print ff decision =
        match decision with
        | DecisionConditional(loc, stmt, truth) ->
            Format.fprintf ff "Decision: @[%a@]: %s@\n" CilPrinter.stmt_abbr stmt (if truth then "true" else "false")
        | DecisionFuncall(loc, instr, varinfo) ->
            Format.fprintf ff "Decision: @[%a@]@\n" CilPrinter.varinfo varinfo

    let to_string ff decision =
        let print_loc ff loc = Format.fprintf ff "(%s,%d,%d)" loc.file loc.line loc.byte in
        match decision with
        | DecisionConditional(loc, stmt, truth) ->
            Format.fprintf ff "DecisionConditional: %a %s@\n" print_loc loc (if truth then "true" else "false")
        | DecisionFuncall(loc, instr, varinfo) ->
            Format.fprintf ff "DecisionFuncall: %a %s@\n" print_loc loc varinfo.vname

    let from_string =
        let re = Str.regexp "\\(.*\\): (\\(.*\\),\\([0-9]*\\),\\([0-9]*\\)) \\(.*\\)" in
        fun cilfile arg ->
            let error arg = FormatPlus.failwith "Error in parsing line %s" arg in
            if Str.string_match re arg 0 then
                try
                    let typ = Str.matched_group 1 arg in
                    let file = Str.matched_group 2 arg in
                    let line = int_of_string (Str.matched_group 3 arg) in
                    let byte = int_of_string (Str.matched_group 4 arg) in
                    let value = Str.matched_group 5 arg in
                    let loc = {
                        Cil.line = line;
                        Cil.file = file;
                        Cil.byte = byte;
                    } in
                    match typ with
                    | "DecisionConditional" -> DecisionConditional (loc, Cil.dummyStmt, match value with "true" -> true | "false" -> false | _ -> error arg)
                    | "DecisionFuncall" -> DecisionFuncall (loc, Cil.dummyInstr, FindCil.global_varinfo_by_name cilfile value)
                    | _ -> error arg
                with Not_found -> error arg
            else error arg

end

include T
