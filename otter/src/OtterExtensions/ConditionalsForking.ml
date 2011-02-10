(** Interceptor that splits jobs that contain conditional values exceeding a certain size into multiple jobs with
    subsets of those conditional values. *)

open DataStructures
open OtterBytes
open OtterCore


let interceptor ?(limit=8) job param k =
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

            let abandoned, states =
                (* recursively split states for each lval with conditional values, e.g., starting with an initial
                   state, split into m states for the first lval, then split into m*n states for the second lval,
                   and so on. *)
                List.fold_left begin fun (abandoned, states) cil_lval ->
                    EfficientSequence.concat_map_fold begin fun abandoned state ->
                        let old_state = state in
                        try
                            (* read the lval, and if conditionals are found, split them and write them back *)
                            let state, lvals, errors = Expression.lval state cil_lval [] in
                            (* TODO: errors should carry the state in which the error occured *)
                            let abandoned = List.rev_append (Statement.errors_to_abandoned_list (job#with_state old_state) errors) abandoned in
                            let state, bytes = MemOp.state__deref state lvals in
                            let splits = split (Bytes.conditional__bytes bytes) in
                            let states =
                                if EfficientSequence.length splits = 1 then
                                    EfficientSequence.singleton state
                                else
                                    EfficientSequence.map begin fun (p, x) ->
                                        let state = MemOp.state__add_path_condition state (Bytes.guard__to_bytes p) false in
                                            MemOp.state__assign state lvals (Bytes.make_Bytes_Conditional x)
                                    end splits
                            in
                            (abandoned, states)

                        with Failure msg ->
                            (* TODO: Failure really needs to go, ditch this once OtterCore has been switched over to using errors *)
                            if !Executeargs.arg_failfast then failwith msg;
                            let job = job#with_state old_state in
                            (Job.Complete (Job.Abandoned (`Failure msg, Job.get_loc job, (job :> Job.job_result)))::abandoned, EfficientSequence.empty)
                    end abandoned states
                end ([], EfficientSequence.singleton job#state) !cil_lvals
            in

            if EfficientSequence.length states = 1 && abandoned = [] then
                (* if no splits and no errors, then just continue the original job *)
                k job param
            else
                (* otherwise, fork the job with the new states and errors *)
                let jobs = EfficientSequence.fold begin fun jobs state ->
                    let job = Job.Active ((job#with_state state)#with_jid (if jobs = [] then job#jid else Counter.next Job.job_counter)) in
                    job::jobs
                end [] states in
                let jobs = List.rev_append abandoned jobs in
                (Job.Fork jobs, param)

        | [] ->
            k job param
