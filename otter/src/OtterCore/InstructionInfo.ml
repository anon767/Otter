(** A component in Job.t that keeps track of which instruction a job has.
 *  Eventually we want to use OtterCFG.Instruction.t.
 *
 *  1. instr_id is updated whenever a job's instrList/stmt is updated. Hence it can be thought as one 
 *     instr_id per execution of instr/stmt at runtime.
 *  2. For jobs a and b, a is the caller of b if a contains a function invocation and immediately leads 
 *     to the creation of b (that is, b contains an instr/stmt within the function being invoked). 
 *     caller_list is therefore the list of instr_id's of callers of a job. This list subsumes 
 *     State.t.callstack.
 *
 *  They are used to keep track of how much time executing an instr/stmt takes. For non-function call 
 *  instr/stmt, it's just the time running the job. For function call instr, it's the time of summing up 
 *  all of its callee jobs.
 *
 *  To update the caller_list, whenever the job is updated with a function call instr, the newly generated 
 *  instr_id is added into the caller_list, and whenever a return stmt is seen, it pops the caller_list. 
 *  To account for built-in functions that do not have return stmts, force_return is used to unconditionally 
 *  pop the caller_list.
 *
 *  This class assumes a function call is returned either by BuiltinFunctions.end_function_call or a Cil.Return statement.
 *
 *  TODO: currently this class is not compatible with MultiOtter, for unknown reason. More exactly, the push/pop events
 *  are not balanced.
 *)

open DataStructures

let instr_id_counter = Counter.make () 

(** [instr_id] is the id of an instruction with context. [caller_list] is a list of instr_ids of the calling context. *)
class t first_stmt :
    object ('self)
        method instr_id : int
        method caller_instr_id : int option
        method caller_list : int list

        method instrList : Cil.instr list
        method with_instrList : Cil.instr list -> 'self

        method stmt : Cil.stmt
        method with_stmt : Cil.stmt -> 'self

        method force_return : 'self

        method become : 'self -> unit
    end
= 
    object (_ : 'self)
        val mutable caller_list = [] (* The root: runtime *)
        val mutable instr_id = Counter.next instr_id_counter

        method instr_id = instr_id
        method caller_instr_id = match caller_list with [] -> None | hd :: _ -> Some hd
        method caller_list = caller_list  (* This caller_list subsumes State.t.callstack *)

        (** [instr]s to execute before moving to the next [stmt] *)
        val mutable instrList = []
        method instrList = instrList
        method with_instrList instrList = 
            let instr_id = Counter.next instr_id_counter in
            let caller_list = match instrList with
                | Cil.Call _ :: _ -> instr_id :: caller_list
                | _ -> caller_list
            in
            {< instr_id = instr_id;
               caller_list = caller_list;
               instrList = instrList; 
             >}

        (* Cannot raise exceptions for empty lists, otherwise MultiOtter fails *)
        val pop = function [] -> [] | _ :: tl -> tl

        (** The next statement the job should execute *)
        val mutable stmt = first_stmt 
        method stmt = stmt
        method with_stmt stmt = 
            let instr_id = Counter.next instr_id_counter in
            let caller_list = match stmt.Cil.skind with 
                | Cil.Return _ -> pop caller_list   
                | _ -> caller_list
            in
            {< instr_id = instr_id;
               caller_list = caller_list;
               stmt = stmt;
             >}

        (** Used by built-in functions to "force" a return without an actual return statement *)
        method force_return = {< caller_list = pop caller_list; >}

        method become (other : 'self) =
            instr_id <- other#instr_id;
            caller_list <- other#caller_list;
            instrList <- other#instrList;
            stmt <- other#stmt;
    end

