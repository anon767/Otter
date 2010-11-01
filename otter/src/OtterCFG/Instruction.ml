open CilUtilities

(* TODO: Refactor OtterCore to use this module for it's program counter, as the one canonical abstraction for Otter.
   In particular, parts of OtterCore.Core.prepare_file should be moved here, as this module relies on the invariants
   set up there. *)

(* Use a private type to disallow direct construction of the type beyond the module below, so that the instruction must
   be constructed via one of the smart constructors which strictly enforce the invariants of this type.
   (see <http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#toc76>). *)
include (struct
    (* An Otter instruction is an optional instruction in a statement in a function in a file; in particular, Cil.Instr
       statements are stored with a list of the current and remaining instructions, whereas other statements are
       stored with an empty instruction list. Note also that the instruction list is compared only by length, since
       some Otter functions mutate the current instruction (e.g.,
       OtterCore.Interceptors.intercept_function_by_name_* ). *)
    type t = {
        file : Cil.file;
        fundec : Cil.fundec;
        stmt : Cil.stmt;
        instrs : Cil.instr list;
    }

    (** Make an instruction. *)
    let make file fundec stmt instrs = match stmt.Cil.skind with
        | Cil.Instr _ when instrs = [] -> invalid_arg "make: instrs must be non-empty when stmt is Cil.Instr"
        | Cil.Instr _ -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs }
        | _ when instrs <> [] -> invalid_arg "make: instrs must be empty when stmt is not Cil.Instr"
        | _ -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs }

    (** Make an instruction from a {!Cil.stmt} only, taking the first instruction if it is a {!Cil.Instr}. *)
    let of_statement_first file fundec stmt = match stmt.Cil.skind with
        | Cil.Instr instrs -> { file = file; fundec = fundec; stmt = stmt; instrs = instrs }
        | _ -> { file = file; fundec = fundec; stmt = stmt; instrs = [] }

    (** Make an instruction from a {!Cil.stmt} only, taking the last instruction if it is a {!Cil.Instr}. *)
    let of_statement_last file fundec stmt = match stmt.Cil.skind with
        | Cil.Instr instrs -> { file = file; fundec = fundec; stmt = stmt; instrs = [ List.hd (List.rev instrs) ] }
        | _ -> { file = file; fundec = fundec; stmt = stmt; instrs = [] }

    (** Make an instruction from a {!Cil.fundec} only, taking the first statement. *)
    let of_fundec file fundec = of_statement_first file fundec (List.hd fundec.Cil.sbody.Cil.bstmts)

    (** Make an instruction by updating {instr} in an instruction. *)
    let with_instrs instruction instrs = match instruction.stmt.Cil.skind, instrs with
        | Cil.Instr instrs', [] -> invalid_arg "with_instrs: instrs must be non-empty"
        | Cil.Instr instrs', _ when List.length instrs > List.length instrs' -> invalid_arg "with_instrs: instrs must not be longer than the Cil.Instr in stmt"
        | Cil.Instr instrs', _ -> { instruction with instrs = instrs' }
        | _, _ -> invalid_arg "with_instrs: instruction.stmt must be Cil.Instr"
end : sig
    type t = private {
        file : Cil.file;
        fundec : Cil.fundec;
        stmt : Cil.stmt;
        instrs : Cil.instr list;
    }
    val make : Cil.file -> Cil.fundec -> Cil.stmt -> Cil.instr list -> t
    val of_statement_first : Cil.file -> Cil.fundec -> Cil.stmt -> t
    val of_statement_last : Cil.file -> Cil.fundec -> Cil.stmt -> t
    val of_fundec : Cil.file -> Cil.fundec -> t
    val with_instrs : t -> Cil.instr list -> t
end)


(** Compare two instructions. *)
let compare x y = if x == y then 0 else
    if x.file == y.file then
        match CilData.CilFundec.compare x.fundec y.fundec with
            | 0 ->
                (* Note that this relies on Cil.computeCFGInfo, called in OtterCore.Core.prepare_file. *)
                begin match Pervasives.compare x.stmt.Cil.sid y.stmt.Cil.sid with
                    | 0 ->
                        (* Instructions are compared by length only, since Otter may mutate the instruction. *)
                        if x.instrs == y.instrs then
                            0
                        else
                            Pervasives.compare (List.length x.instrs) (List.length y.instrs)
                    | i -> i
                end
            | i -> i
    else
        (* TODO: it is possible for fileName's to be the same iff the same file were parsed twice; need some way to
           attach a unique id to the file. *)
        Pervasives.compare x.file.Cil.fileName y.file.Cil.fileName


(** Compare two instructions for equality. *)
let equal x y = compare x y = 0


(** Find the successors for an instruction. *)
let successors ({ file = file; fundec = fundec; stmt = stmt; instrs = instrs } as instruction) = match instrs with
    | _::[] | [] ->
        (* last Cil.instr in a Cil.Instr, or a non-Cil.Instr *)
        List.map (of_statement_first file fundec) stmt.Cil.succs
    | _::rest ->
        (* the remaining Cil.instr in a Cil.Instr *)
        [ with_instrs instruction rest ]


(** Find the predecessors for an instruction. *)
let predecessors ({ file = file; fundec = fundec; stmt = stmt; instrs = instrs } as instruction) = match stmt.Cil.skind with
    | Cil.Instr instrs' when (List.length instrs) = (List.length instrs') ->
        (* first Cil.instr in a Cil.Instr or non-Cil.Instr *)
        List.map (of_statement_last file fundec) stmt.Cil.preds
    | Cil.Instr instrs' ->
        (* some instruction in the middle of a Cil.Instr *)
        [ with_instrs instruction ((List.nth instrs' (List.length instrs))::instrs) ]
    | _ ->
        (* non-Cil.Instr *)
        List.map (of_statement_last file fundec) stmt.Cil.preds


(** Find the all the call sites of this instruction (if it is the first instruction of its function). *)
let call_sites =
    let memotable = Hashtbl.create 0 in
    fun ({ file = file; fundec = fundec } as instruction) ->
        if equal instruction (of_fundec file fundec) then begin
            try
                Hashtbl.find memotable (file, fundec)
            with Not_found ->
                let call_sites = ref [] in

                (* iterate over caller functions *)
                let callers = CilCallgraph.find_callers file fundec in
                List.iter begin fun caller ->
                    (* iterate over statements in caller functions *)
                    ignore begin Cil.visitCilFunction begin object
                        inherit Cil.nopCilVisitor
                        method vstmt stmt = match stmt.Cil.skind with
                            | Cil.Instr instrs ->
                                (* iterate over instructions, which contains Cil.Call *)
                                let rec traverse = function
                                    | (Cil.Call (_, fexp, _, _))::rest as instrs ->
                                        (* find Cil.Call that calls the target *)
                                        let call_targets = CilCallgraph.resolve_exp_to_fundecs file fexp in
                                        if List.memq fundec call_targets then
                                            call_sites := (make file caller stmt instrs)::!call_sites;
                                        traverse rest
                                    | _::rest ->
                                        traverse rest
                                    | [] ->
                                        Cil.SkipChildren
                                in
                                traverse instrs
                            | _ ->
                                Cil.SkipChildren
                    end end caller end
                end callers;

                Hashtbl.add memotable (file, fundec) !call_sites;
                !call_sites
        end else
            [] (* or raise some exception? *)


(** Find the all the (first instruction of the) call targets of this instruction. *)
let call_targets =
    let memotable = Hashtbl.create 0 in
    fun { file = file; instrs = instrs } ->
        match instrs with
            | (Cil.Call (_, fexp, _, _))::rest ->
                begin
                    try
                        Hashtbl.find memotable (file, fexp)
                    with Not_found ->
                        (* iterate over callee functions and extract the first statement *)
                        let callees = CilCallgraph.resolve_exp_to_fundecs file fexp in
                        let call_targets = List.map (of_fundec file) callees in

                        Hashtbl.add memotable (file, fexp) call_targets;
                        call_targets
                end
            | _ ->
                [] (* or raise some exception? *)

