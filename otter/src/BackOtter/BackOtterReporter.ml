open OcamlUtilities
open OtterCore
open OtterReporter

module Reporter = ErrorReporter.Make (BackOtterErrors)

let arg_finalizer = ref ""

class ['self] t ?max_steps ?max_paths ?max_abandoned () = object
    inherit ['self] Reporter.t ?max_steps ?max_paths ?max_abandoned () as super

    method report results =
        List.iter begin function
            | Job.Abandoned (`TargetReached _), job ->
                (* Run a user-defined function after `TargetReached.
                 * The function can evaluate symbolic values (e.g., argvs) under the program state where TargetReached occurs. *)
                if (!arg_finalizer) = "" then () else begin try
                    let finalizer = CilUtilities.FindCil.fundec_by_name job#file (!arg_finalizer) in
                    let job = MemOp.state__start_fcall job State.Runtime finalizer [] in
                    let job = job#with_stmt (List.hd finalizer.Cil.sallstmts) in
                    let job = job#with_instrList [] in
                    let queue = BackOtterQueue.get_default_fqueue () in
                    let queue = queue#put job in
                    let interceptor =
                        let (>>>) = Interceptor.(>>>) in
                            BackOtterInterceptor.set_output_formatter_interceptor
                        >>> BackOtterInterceptor.jobinit_interceptor
                        >>> Interceptor.function_pointer_interceptor
                        >>> BackOtterBuiltinFunctions.interceptor
                        >>> BuiltinFunctions.interceptor
                    in
                    let step job = interceptor job Statement.step in
                    let reporter = new Reporter.t () in
                    ignore (OtterDriver.Driver.main_loop step queue reporter)
                with Not_found ->
                    Output.set_mode Output.MSG_ERROR;
                    Output.printf "Finalizer %s not found@\n" (!arg_finalizer)
                end
            | Job.Truncated (`SummaryAbandoned reason), job ->
                Output.set_mode Output.MSG_ERROR;
                Output.printf "@[<h>Truncated \"@[<h>%a@]\"@ occurs at @[%a@].@\n"
                    BackOtterErrors.printer reason Printcil.loc (Job.get_loc job)
            | _ ->
                ()
        end results;
        super#report results
end

let options = [
    "--backotter-finalizer",
        Arg.Set_string arg_finalizer,
        Printf.sprintf "<fname> Set the finalizer to be run after `TargetReached (default: %s)" (!arg_finalizer);
] 
