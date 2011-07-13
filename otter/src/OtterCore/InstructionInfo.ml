(** A component in Job.t that keeps track of which instruction a job has.
 *  Eventually we want to use OtterCFG.Instruction.t.
 *)
class t first_stmt :
    object ('self)
        method instrList : Cil.instr list
        method with_instrList : Cil.instr list -> 'self

        method stmt : Cil.stmt
        method with_stmt : Cil.stmt -> 'self

        method become : 'self -> unit
    end
=
    object (_ : 'self)

        (** [instr]s to execute before moving to the next [stmt] *)
        val mutable instrList = []
        method instrList = instrList
        method with_instrList instrList = {< instrList = instrList >}

        (** The next statement the job should execute *)
        val mutable stmt = first_stmt 
        method stmt = stmt
        method with_stmt stmt = {< stmt = stmt >}

        method become (other : 'self) =
            instrList <- other#instrList;
            stmt <- other#stmt;
    end

