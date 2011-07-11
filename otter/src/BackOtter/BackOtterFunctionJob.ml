open CilUtilities
open OtterCore
open OtterJob

let concrete_globals_paths = ref []

(* Note: this class inherits directly from OtterCore.Job.t *)
class ['abandoned, 'truncated] t file ?(points_to=(!FunctionJob.default_points_to) file) fn :
    object ('self)
        inherit ['abandoned, 'truncated] OtterCore.Job.t
        inherit BackOtterJobExtension.t
    end
=
    object (self : 'self)
        inherit ['abandoned, 'truncated] OtterCore.Job.t file fn as job_super
        inherit BackOtterJobExtension.t as b_super

        initializer
            let job = self in

            let symbolic_globals, concrete_globals = List.fold_left begin fun (symbolic_globals, concrete_globals) g -> 
                match g with
                | Cil.GVar _ | Cil.GVarDecl _ ->
                    let loc = Cil.get_globalLoc g in
                    (* hoisted strings have unknown location, and they have to be initialized together with some other globals here *)
                    if loc == Cil.locUnknown || (List.fold_left (fun b reg -> b || (Str.string_match reg loc.Cil.file 0)) false (!concrete_globals_paths)) then
                        symbolic_globals, (g :: concrete_globals)
                    else
                        (g :: symbolic_globals), concrete_globals
                | _ -> symbolic_globals, concrete_globals
            end ([],[]) file.Cil.globals in

            let symbolic_globals = List.rev symbolic_globals in
            let concrete_globals = List.rev concrete_globals in

            (* Setup these globals concretely *)
            let job = FileJob.init_globalvars job concrete_globals in

            (* Setup these globals using SymbolicPointers *)
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
                    let deferred job = FunctionJob.init_bytes_with_pointers job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                    let job, lval_block = SymbolicPointers.init_lval_block job v deferred in
                    job#with_state { job#state with State.global = State.VarinfoMap.add v lval_block job#state.State.global }
                | _ ->
                    job
            end job symbolic_globals in

            (* then, setup function arguments *)
            (* TODO: handle varargs *)
            let job, rev_args_bytes = List.fold_left begin fun (job, args_bytes) v ->
                let job, bytes = FunctionJob.init_bytes_with_pointers job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                (job, bytes::args_bytes)
            end (job, []) fn.Cil.sformals in

            (* finally, enter the function *)
            let job = MemOp.state__start_fcall job State.Runtime fn (List.rev rev_args_bytes) in

            self#become job

        method append_decision_path decision = 
            if self#enable_record_decisions then
                let job = job_super#append_decision_path decision in
                job#postprocess_append_decision_path decision
            else self

        method become (other : 'self) =
            job_super#become other;
            b_super#become other

    end

let options = [
    "--backotter-concrete-globals-location",
        Arg.String (fun str -> concrete_globals_paths := (Str.regexp_string str)::(!concrete_globals_paths)),
        "<path> Specify location whose globals are initialized concretely.";
]

