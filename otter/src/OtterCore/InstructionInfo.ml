(** A component in Job.t that keeps track of which instruction a job has.
 *  Eventually we want to use OtterCFG.Instruction.t.
 *
 *  caller_list is a list of [Cil.location]s of call sites of a job. This list subsumes [State.t.callstack].
 *
 *  It is used to keep track of how much time executing an instr/stmt takes. For non-function call 
 *  instr/stmt, it's just the time running the job. For function call instr, it's the time of summing up 
 *  all of its callee jobs.
 *
 *)

open DataStructures

let instr_id_counter = Counter.make () 

class t first_stmt :
    object ('self)
        method caller_list : Cil.location list

        method cur_call : Cil.location

        method instrList : Cil.instr list
        method with_instrList : Cil.instr list -> 'self

        method stmt : Cil.stmt
        method with_stmt : Cil.stmt -> 'self

        method push_caller_list : 'self
        method pop_caller_list : 'self

        method become : 'self -> unit
    end
= 
    object (_ : 'self)
        val mutable caller_list = [] (* The root: runtime *)

        method caller_list = caller_list  (* This caller_list subsumes State.t.callstack *)

        (** Most recently seen location of function call 
         *  This is used to update caller_list once we step into a function.
         *  TODO: not very elegant. Reimplement this in a better way. *)
        val mutable cur_call = Cil.locUnknown
        method cur_call = cur_call

        (** [instr]s to execute before moving to the next [stmt] *)
        val mutable instrList = []
        method instrList = instrList
        method with_instrList instrList = 
            let cur_call = match instrList with
                | Cil.Call (_,_,_,loc) :: _ -> loc 
                | _ -> cur_call
            in
            {< cur_call = cur_call;
               instrList = instrList >}

        (** The next statement the job should execute *)
        val mutable stmt = first_stmt 
        method stmt = stmt
        method with_stmt stmt = {< stmt = stmt >}

        (** Called by MemOp.state__start_fcall *)
        method push_caller_list = {< caller_list = cur_call :: caller_list >}

        (** Called by MemOp.state__end_fcall *)
        method pop_caller_list = {< caller_list = List.tl caller_list >}

        method become (other : 'self) =
            caller_list <- other#caller_list;
            cur_call <- other#cur_call;
            instrList <- other#instrList;
            stmt <- other#stmt;
    end

