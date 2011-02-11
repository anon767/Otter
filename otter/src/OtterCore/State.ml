open DataStructures
open OtterBytes
open Bytes

module VarinfoMap = Map.Make (CilUtilities.CilData.CilVar)

module MallocMap = Map.Make (CilUtilities.CilData.Malloc)

module IndexMap = Map.Make (struct
	type t = int
	let compare (a : int) (b : int) = Pervasives.compare a b
end)

module MemoryBlockMap = Map.Make (struct
	type t = memory_block
	let compare (a:t) b = Pervasives.compare a.memory_block_id b.memory_block_id
end)

module VargsMap = Map.Make (struct
	type t = bytes
	let compare : t -> t -> int = Pervasives.compare
end)

(* TODO: move this elsewhere; or perhaps use Sys.catch_break *)
exception SignalException of string


(** A calling context may either be the symbolic executor, represented by
		[Runtime], or from another function in the source code, represented
		either by a tuple [Source (destOpt,callStmt,callInstr,nextStmt)] if the
		function returns or [NoReturn (callStmt,callInstr)] if the function doesn't return.
		[nextStmt] is the [stmt] to execute after the call returns; [callStmt]
		and [callInstr] are the function call statement and instruction;
		and [destOpt] is [None] if we ignore the result of the call, or it is
		[Some Cil.lval], which means we should assign the result there. *)
type callingContext =
    | Runtime
    | Source of (Cil.lval option * Cil.stmt * Cil.instr * Cil.stmt)
    | NoReturn of (Cil.stmt * Cil.instr)



(** Lazy memory stack frame. *)
type 'job memory_frame =
	('job, lval_block) Deferred.t VarinfoMap.t

(** Lazy memory heap. *)
type 'job memory_heap =
	('job, bytes) Deferred.t MemoryBlockMap.t

(** Symbolic execution state details. *)
type 'job state =
	{
		global : 'job memory_frame;                  (* Map global lvals to blocks *)
		formals : 'job memory_frame list;            (* Map formal lvals to blocks *)
		locals : 'job memory_frame list;             (* Map local lvals to blocks *)
		(* TODO: move aliases/mallocs into a class mix-in for SymbolicPointers *)
		aliases : memory_block list VarinfoMap.t; (* Map from varinfos to aliased blocks, e.g., from unknown call stack recursion *)
		mallocs : memory_block list MallocMap.t; (* Map from malloc sites to aliased blocks from unknown allocation *)
		callstack : Cil.fundec list;            (* Function call stack *)
		block_to_bytes : 'job memory_heap;
		path_condition : bytes list;
		path_condition_tracked : bool list;
		callContexts : callingContext list;
		(** The last element of callstack is the function at which the
				executor started execution. The last element of callContexts
				is [Runtime]. Other than that, the nth element of callstack is
				the fundec called by the [Instr] at the nth position in
				callContexts. *)
		stmtPtrs : callingContext IndexMap.t;     (* Pointers into code.  Used for longjump. *)

		va_arg : bytes list list;			(* A stack of va_arg *)
		va_arg_map : bytes list VargsMap.t;
	}


(** Create an empty symbolic execution state.

    Note: subclasses of [t] will not be subtypes of [t], due to the the lazy memory representation using
    self-recursive Deferred.t. Basically, lazy memory uses ['self -> 'self] functions, where ['self] is the type of
    the class. Subtypes of functions have to be contravariant in the parameters and covariant in the return, but
    a subclass ['sub] may extend ['self] with additional methods, and so lazy memory in the subclass will be of type
    ['sub -> 'sub], which is not a subtype of ['self -> 'self].

    A simplified of the above is:
{[
    class a = object (self : 'self)
        val deferred : 'self -> 'self = fun x -> x
        method defer f = {< deferred = f >}
        method force = deferred self
    end

    class b = object
        inherit a
        method bar = 1
    end

    (* [new a; (new b :> a)] will fail and show that b is not a subtype of a *)
]}
*)
class t = object (_ : 'self)
    val mutable state = {
        global = (VarinfoMap.empty : 'self memory_frame);
        formals = [(VarinfoMap.empty : 'self memory_frame)];
        locals = [(VarinfoMap.empty : 'self memory_frame)]; (* permit global init with another global *)
        aliases = VarinfoMap.empty;
        mallocs = MallocMap.empty;
        callstack = [];
        block_to_bytes = (MemoryBlockMap.empty : 'self memory_heap);
        path_condition = [];
        path_condition_tracked = [];
        (*return = None;*)
        callContexts = [];
        stmtPtrs = IndexMap.empty;
        va_arg = [];
        va_arg_map = VargsMap.empty;
    }

    (** Get the symbolic execution state *)
    method state = state

    (** Set the symbolic execution state *)
    method with_state state = {< state = state >}

    (** [x#become y] destructively copies all instance variables from [y] to [x]. This should be used sparingly,
        typically only in object initializers.

        {b Subclasses are responsible for providing [#become] that calls [#become] on all superclasses, in addition to
        copying their own instance variables.}

        Unfortunately, Ocaml's object initializers returns [unit], not ['self], and so can't perform initialization via
        the functional update syntax. As a workaround, we'll declare all instance variables mutable, and provide
        [#become] which simply copies all instance variables from another object of the same type. Thus, initializers
        can be used to perform further initialization using the pattern:
{[
    initializer
        let x = self in;
        let x = some_operation x in
        ...
        self#become x
]}
    *)
    method become (other : 'self) =
        state <- other#state
end

