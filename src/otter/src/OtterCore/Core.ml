open CilUtilities

exception CIL_consistency_error

(* set up the file for symbolic execution *)
let prepare_file file =
    (* catch consistency errors before doing anything else *)
    if not (Check.checkFile [] file) then begin
        flush !Errormsg.logChannel;
        raise CIL_consistency_error
    end;
    (* based on Cilly.makeCFGFeature.fd_doit *)
    HoistStringLiterals.apply file;
    WidenBitfields.apply file;
    Partial.calls_end_basic_blocks file;
    Partial.globally_unique_vids file;
    Cil.iterGlobals file begin function
        | Cil.GFun(fd,_) ->
            Cil.prepareCFG fd; (* reduce switch/break/default/continue to if/goto *)
            ignore (Cil.computeCFGInfo fd false) (* false: per-function statement ids, required for Coverage *)
        | _ ->
            ()
    end;

    if !Executeargs.arg_noinit_unreachable_globals then
        Coverage.computeReachableCode file;

    Coverage.prepare_file file

