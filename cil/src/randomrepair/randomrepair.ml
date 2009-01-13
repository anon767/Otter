
(* options: by default, run for 10 seconds and a minimum of 100 iterations *)
let opt_timelimit = ref 10.0
let opt_iterlimit = ref max_int
let opt_itermin = ref 100
let opt_showcandidates = ref false
let opt_benchmark = ref ""


let repair fout score file func =
    let starttime = Sys.time () in
    let rec repair (patcher : Randommod.patcher) score iter candidate stats =
        (* try to repair *)
        let (patcher, repaired, _) = patcher#take () in
        repaired.Cil.svar.Cil.vname <- (Format.sprintf "__randomrepair__%d__%s" iter func.Cil.svar.Cil.vname);
        if !opt_showcandidates then
            ignore (Pretty.printf "%a" (Cil.printGlobal (Cil.descriptiveCilPrinter :> Cil.cilPrinter))  (Cil.GFun(repaired, Cil.locUnknown)));

        (* compare it against candidate *)
        let (_, _, new_score) = Inconsistency.evaluate file repaired in
        let (score, candidate) = if new_score < score then begin (* found a better program *)
            Format.fprintf fout "@\n@[(%5.1f seconds) Candidate %d score: %d@]@?" (Sys.time () -. starttime) iter new_score;
            (new_score, repaired)
        end else begin (* not any better, try again *)
            Format.fprintf fout ".@?";
            (score, candidate)
        end in
        let stats = stats @ [ new_score ] in
        (* check time/iteration limits *)
        let elapsed = Sys.time () -. starttime in
        if score = 0 or (iter >= !opt_itermin &
                (elapsed >= !opt_timelimit or iter >= !opt_iterlimit))
        then begin
            Format.fprintf fout "@\n";
            (score, iter, elapsed, candidate, stats)
        end else
            repair patcher score (iter + 1) candidate stats
    in
    let patcher = new Randommod.patcher ~filters:[new Randommod.functionFilter ["__BREAKPT"; "__ASSUME"; "__ASSERT"; "AND"; "OR"; "NOT"]] func in
    repair patcher score 1 func [ score ]



let apply fout file func =
    let (_, _, score) = Inconsistency.evaluate file func in
    if score = 0 then begin
        Format.fprintf fout "Program is consistent.@\n";
        (0, 0, 0.0, func, [])
    end else begin
        Format.fprintf fout "Program is inconsistent (score: %d). Attempting to repair...@\n" score;
        repair fout score file func
    end



let find_function file fname =
    let rec find_function = function
        | Cil.GFun(fundec, _)::_ when fundec.Cil.svar.Cil.vname = fname -> fundec
        | _::tl -> find_function tl
        | _ -> failwith "main function not found!"
    in
    find_function file.Cil.globals



let do_benchmark file func =
    let dirname = !opt_benchmark in
    if Sys.file_exists dirname then
        failwith (Format.sprintf "%s already exists!" dirname);
		let _ = Unix.mkdir dirname 0o777 in
    (*let dir = Unix.mkdir dirname 0o777 in*)
        failwith ("TODO: call evaluate, store results in source-code to (score, iter, elapsed) map,@\n"
                 ^"and write out each source-code out to files, and tabulate the results")




let do_one file func =
    let ff = Format.std_formatter in
    let (score, iter, elapsed, candidate, stats) = apply ff file func in
    Format.fprintf ff "Most consistent candidate (score: %d):@\n  @[%s@]@\n" score
            (Pretty.sprint max_int (Cil.descriptiveCilPrinter#pGlobal  () (Cil.GFun(candidate, Cil.locUnknown))));
    Format.fprintf ff "Score history: @[%a@]@\nMean iteration duration: %5.1f seconds@\n"
            (fun ff -> List.iter (fun n -> Format.fprintf ff "@ %d," n)) stats
            (elapsed /. (float_of_int iter));
    func.Cil.sbody <- candidate.Cil.sbody



let doit file =
    Random.self_init ();
    let main_func = find_function file "main" in
    if !opt_benchmark = "" then
        do_one file main_func
    else
        do_benchmark file main_func



let feature = {
    Cil.fd_name = "randomrepair";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "attempt to randomly repair an inconsistent program.";
    Cil.fd_extraopt = [
        ("--randomrepair-timelimit", Arg.Set_float(opt_timelimit),
            "<f> maximum time in seconds to try repairs (default 10.0)");
        ("--randomrepair-iterlimit", Arg.Set_int(opt_iterlimit),
            "<n> maximum number of repairs to try (default unlimited)");
        ("--randomrepair-itermin", Arg.Set_int(opt_itermin),
            "<n> minimum number of repairs to try (default 100)");
        ("--randomrepair-showcandidates", Arg.Set(opt_showcandidates),
            " show candidate source code as they are being tested");
        ("--randomrepair-benchmark", Arg.Set_string(opt_benchmark),
            "<dir> run a benchmark of 100 repairs, and store results in directory")
    ];
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}
