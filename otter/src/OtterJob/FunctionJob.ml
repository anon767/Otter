(** Module for start-in-the-middle symbolic execution jobs. *)

open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore


let points_tos = [
    "olf", CilUtilities.CilPtranal.points_to;
    "naive", CilUtilities.CilPtranal.naive_points_to;
    "unsound", CilUtilities.CilPtranal.unsound_points_to;
    "unsound-typed-void", CilUtilities.CilPtranal.unsound_typed_void_points_to;
]
let default_points_to = ref CilUtilities.CilPtranal.points_to

let default_uninit_void = ref false



(** Create a new symbolic executor job starting at a given function, and using {!init_bytes_with_pointers} to initialize
    the symbolic job.

        @param file is the file to symbolically execute
        @param uninit_void optionally indicates whether targets of void * pointers should be uninitialized (see
                {!init_bytes_with_pointers})
        @param points_to is a function for computing pointer targets to be passed to {!init_bytes_with_pointers}
                (default:[CilPtranal.points_to file])
        @param fn is list the function at which to begin symbolic execution
        @return [OtterCore.Job.job] the created job
*)
class ['abandoned, 'truncated] t file ?uninit_void ?(points_to=(!default_points_to) file) fn =
    object (self : 'self)
        inherit ['abandoned, 'truncated] OtterCore.Job.t file fn
        initializer
            let job = self in

            (* first, setup global variables *)
            let job = List.fold_left begin fun job g -> match g with
                | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _) when Cil.isFunctionType v.Cil.vtype ->
                    (* skip function prototypes; they're not variables *)
                    job
                | Cil.GVarDecl (v, _) when CilData.CilVar.is_const v ->
                    (* forward declaration *)
                    SymbolicPointers.init_const_global job v None
                | Cil.GVar (v, { Cil.init = Some init }, _) when CilData.CilVar.is_const v ->
                    SymbolicPointers.init_const_global job v (Some init)
                | Cil.GVarDecl (v, _) | Cil.GVar (v, _, _) when not (State.VarinfoMap.mem v job#state.State.global) ->
                    let deferred job = SymbolicPointers.init_bytes_with_pointers ~uninit_void:(!default_uninit_void) job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                    let job, lval_block = SymbolicPointers.init_lval_block job v deferred in
                    job#with_state { job#state with State.global = State.VarinfoMap.add v lval_block job#state.State.global }
                | _ ->
                    job
            end job file.Cil.globals in

            (* then, setup function arguments *)
            (* TODO: handle varargs *)
            let job, rev_args_bytes = List.fold_left begin fun (job, args_bytes) v ->
                let job, bytes = SymbolicPointers.init_bytes_with_pointers ~uninit_void:(!default_uninit_void) job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                (job, bytes::args_bytes)
            end (job, []) fn.Cil.sformals in

            (* finally, enter the function *)
            let job = MemOp.state__start_fcall job State.Runtime fn (List.rev rev_args_bytes) in

            self#become job
    end

let options = [
    "--function-job-points-to",
        Arg.Symbol (fst (List.split points_tos), fun name -> default_points_to := List.assoc name points_tos),
        " Set the default points_to analysis (default: "
            ^ (fst (List.find (fun (_, x) -> x == !default_points_to) points_tos)) ^ ")";
    if !default_uninit_void then
        "--function-job-not-uninit-void",
        Arg.Clear default_uninit_void,
        " Specify that targets of void * pointers should be initialized"
    else
        "--function-job-uninit-void",
        Arg.Set default_uninit_void,
        " Specify that targets of void * pointers should be uninitialized";
]

