(** Module for start-in-the-middle symbolic execution jobs. *)

open OcamlUtilities
open CilUtilities
open OtterBytes
open OtterCore


let points_tos = [
    "regular", CilUtilities.CilPtranal.points_to;
    "naive", CilUtilities.CilPtranal.naive_points_to;
    "unsound", CilUtilities.CilPtranal.unsound_points_to;
]
let default_points_to = ref CilUtilities.CilPtranal.points_to


(**/**)
let fold_struct f acc compinfo =
    List.fold_left f acc compinfo.Cil.cfields

let fold_array f acc len_opt =
    begin match len_opt with
        | Some len ->
            begin match Cil.isInteger len with
                | Some n ->
                    let rec fold_array acc i index =
                        if Int64.compare i n == 0 then
                            Some acc
                        else
                            let acc = f acc index in
                            fold_array acc (Int64.succ i) (Cil.increm index 1)
                    in
                    fold_array acc Int64.zero Cil.zero
                | None ->
                    (* non-constant length *)
                    None
            end
        | None ->
            (* no length *)
            None
    end
(**/**)


(** Initialize program values symbolically, using {!SymbolicPointers.init_pointer} to initialize pointers.

        @param scheme optionally indicates the scheme to initialize symbolic pointers (see {!SymbolicPointers.init_pointer})
        @param job is the symbolic executor job in which to initialize the variable
        @param typ is the type of the value to initialize
        @param points_to is a function for computing pointer targets to be passed to {!SymbolicPointers.init_pointer}
        @param exps is list of expressions, which are joined to compute the program value
        @return [(job, bytes)] the updated symbolic job and the initialized variable
*)
let rec init_bytes_with_pointers ?scheme job typ points_to exps = match Cil.unrollType typ with
    | Cil.TPtr (Cil.TFun _, _) ->
        (* TODO: use a points to analysis to resolve the target functions *)
        let bytes = Bytes.bytes__symbolic (Cil.bitsSizeOf typ / 8) in
        let null_bytes = Bytes.bytes__zero in
        let bytes = Bytes.make_Bytes_Conditional (Bytes.IfThenElse (
                Bytes.guard__symbolic (), Bytes.conditional__bytes bytes, Bytes.conditional__bytes null_bytes))
        in
        (job, bytes)

    | Cil.TPtr _ ->
        (* for pointers, generate the pointer *)
        let init_target typ vars mallocs job =
            let var_exps = List.map (fun v -> Cil.Lval (Cil.var v)) vars in
            let malloc_exps =
                if mallocs <> [] then
                    List.map (fun exp -> Cil.Lval (Cil.Mem exp, Cil.NoOffset)) exps
                else
                    []
            in
            let exps = var_exps @ malloc_exps in
            if exps <> [] then
                init_bytes_with_pointers ?scheme job typ points_to exps
            else
                let size = Cil.bitsSizeOf typ / 8 in
                (job, Bytes.bytes__symbolic size)
        in

        (* give it a name *)
        let block_name = FormatPlus.as_string Printcil.exp (List.hd exps) in

        (* finally, make the pointer *)
        SymbolicPointers.init_pointer ?scheme job points_to exps block_name init_target

    | Cil.TComp (compinfo, _) when compinfo.Cil.cstruct ->
        (* for structs, initialize and iterate over the fields *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        fold_struct begin fun (job, bytes) field ->
            let field_offset = Cil.Field (field, Cil.NoOffset) in
            let field_exps = List.map begin function
                | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval field_offset lval)
                | _ -> failwith "are there any other Cil.exp that can have type Cil.TComp?"
            end exps in
            let job, field_bytes = init_bytes_with_pointers ?scheme job (Cil.typeOffset typ field_offset) points_to field_exps in
            let offset, size = Cil.bitsOffset typ field_offset in
            let offset = Bytes.int_to_bytes (offset / 8) in
            let size = size / 8 in
            let (), bytes = BytesUtility.bytes__write () bytes offset size field_bytes in
            (job, bytes)
        end (job, bytes) compinfo

    | Cil.TArray (el_typ, len_opt, _) ->
        (* for arrays, initialize each element of the array *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        let result_opt = fold_array begin fun (job, bytes) index ->
            let el_offset = Cil.Index (index, Cil.NoOffset) in
            let el_exps = List.map begin function
                | Cil.Lval lval -> Cil.Lval (Cil.addOffsetLval el_offset lval)
                | _ -> failwith "are there any other Cil.exp that can have type Cil.TArray?"
            end exps in
            let job, el_bytes = init_bytes_with_pointers ?scheme job el_typ points_to el_exps in
            let offset, size = Cil.bitsOffset typ el_offset in
            let offset = Bytes.int_to_bytes (offset / 8) in
            let size = size / 8 in
            let (), bytes = BytesUtility.bytes__write () bytes offset size el_bytes in
            (job, bytes)
        end (job, bytes) len_opt in
        begin match result_opt with
            | Some result -> result
            | None -> (job, bytes)
        end

    | Cil.TComp (compinfo, _) when not compinfo.Cil.cstruct ->
        (* we can't handle unions *)
        FormatPlus.failwith "TODO: init_bytes_with_pointers: handle unions: %a" Printcil.typ typ

    | typ when Cil.isArithmeticType typ ->
        (* arithmetic values are just that *)
        let size = Cil.bitsSizeOf typ / 8 in
        let bytes = Bytes.bytes__symbolic size in
        (job, bytes)

    | typ ->
        FormatPlus.failwith "TODO: init_bytes_with_pointers: unhandled type: %a" Printcil.typ typ



(** Create a new symbolic executor job starting at a given function, and using {!init_bytes_with_pointers} to initialize
    the symbolic job.

        @param scheme optionally indicates the scheme to initialize symbolic pointers (see {!init_bytes_with_pointers})
        @param file is the file to symbolically execute
        @param points_to is a function for computing pointer targets to be passed to {!init_bytes_with_pointers}
                (default:[CilPtranal.points_to file])
        @param fn is list the function at which to begin symbolic execution
        @return [OtterCore.Job.job] the created job
*)
class t file ?scheme ?(points_to=(!default_points_to) file) fn =
    object (self : 'self)
        inherit OtterCore.Job.t file fn
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
                    let deferred job = init_bytes_with_pointers ?scheme job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                    let job, lval_block = SymbolicPointers.init_lval_block job v deferred in
                    job#with_state { job#state with State.global = State.VarinfoMap.add v lval_block job#state.State.global }
                | _ ->
                    job
            end job file.Cil.globals in

            (* then, setup function arguments *)
            (* TODO: handle varargs *)
            let job, rev_args_bytes = List.fold_left begin fun (job, args_bytes) v ->
                let job, bytes = init_bytes_with_pointers ?scheme job v.Cil.vtype points_to [ (Cil.Lval (Cil.var v)) ] in
                (job, bytes::args_bytes)
            end (job, []) fn.Cil.sformals in

            (* finally, enter the function *)
            let job = MemOp.state__start_fcall job State.Runtime fn (List.rev rev_args_bytes) in

            self#become job
    end

let options = [
    "--points-to-analysis",
        Arg.Symbol (fst (List.split points_tos), fun name -> default_points_to := List.assoc name points_tos),
        "<points_to> Set the default points_to analysis (default: regular)";
]

