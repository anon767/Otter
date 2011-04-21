(** Interceptor that splits jobs that contain conditional values exceeding a certain size into multiple jobs with
    subsets of those conditional values. *)

open DataStructures
open OtterBytes
open OtterCore


let interceptor ?(limit=8) job k =
    match job#instrList with
        | instr::_  ->
            (* grab all lvals *)
            let cil_lvals = ref [] in
            ignore begin Cil.visitCilInstr begin object
                inherit Cil.nopCilVisitor
                method vexpr = function
                    | Cil.AddrOf _ | Cil.StartOf _ -> Cil.SkipChildren
                    | _ -> Cil.DoChildren
                method vlval cil_lval =
                    if not (Cil.isFunctionType (Cil.typeOfLval cil_lval)) then
                        cil_lvals := cil_lval::!cil_lvals;
                    Cil.DoChildren
            end end instr end;

            (* helper to split conditionals *)
            let split c =
                let rec split p = function
                    | Bytes.IfThenElse (g, x, y) ->
                        let gx = Bytes.guard__and p g in
                        let gy = Bytes.guard__and_not p g in
                        let px, cx, sx, rx = split gx x in
                        let py, cy, sy, ry = split gy y in
                        let results = EfficientSequence.append rx ry in
                        if sx + sy > limit then
                            if sx > sy then
                                (Bytes.guard__and_not py gx, y, sy, EfficientSequence.cons (gx, x) results)
                            else
                                (Bytes.guard__and_not px gy, x, sx, EfficientSequence.cons (gy, y) results)
                        else
                            (Bytes.guard__and px py, Bytes.IfThenElse (g, cx, cy), sx + sy, results)
                    | Bytes.Unconditional _ as c ->
                        (Bytes.guard__true, c, 1, EfficientSequence.empty)
                in
                let p, c, _, results = split Bytes.guard__true c in
                EfficientSequence.cons (p, c) results
            in

            (* recursively split jobs for each lval with conditional values, e.g., starting with an initial
               job, split into m jobs for the first lval, then split into m*n jobs for the second lval,
               and so on. *)
            List.fold_left begin fun job cil_lval ->
                try
                    (* read the lval, and if conditionals are found, split them and write them back *)
                    let job, lvals = Expression.lval job cil_lval in
                    let job, bytes = MemOp.state__deref job lvals in
                    let splits = split (Bytes.conditional__bytes bytes) in
                    if EfficientSequence.length splits = 1 then
                        job
                    else
                        let job, (p, x) = (job : _ #Info.t)#fork (EfficientSequence.to_list splits) in
                        let job = MemOp.state__add_path_condition job (Bytes.guard__to_bytes p) false in
                        let job = MemOp.state__assign job lvals (Bytes.make_Bytes_Conditional x) in
                        job

                with Failure msg ->
                    (* TODO: Failure really needs to go, ditch this once OtterCore has been switched over to using errors *)
                    if !Executeargs.arg_failfast then failwith msg;
                    (job : _ #Info.t)#finish (Job.Abandoned (`Failure msg))
            end job !cil_lvals

        | [] ->
            k job
