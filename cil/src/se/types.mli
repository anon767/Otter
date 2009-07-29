type operator =
    OP_PLUS
  | OP_SUB
  | OP_MULT
  | OP_DIV
  | OP_MOD
  | OP_LSL
  | OP_LSR
  | OP_LT
  | OP_GT
  | OP_LE
  | OP_GE
  | OP_EQ
  | OP_NE
  | OP_BAND
  | OP_BXOR
  | OP_BOR
  | OP_LAND
  | OP_LOR
  | OP_SX
  | OP_UMINUS
  | OP_BNOT
  | OP_LNOT
type symbol = { symbol_id : int; }
and indicator =
    Indicator of int
  | Indicator_Not of indicator
  | Indicator_And of indicator * indicator
and byte =
    Byte_Concrete of char
  | Byte_Symbolic of symbol
  | Byte_Bytes of bytes * int
and bytes =
    Bytes_Constant of Cil.constant
  | Bytes_ByteArray of byte ImmutableArray.t
  | Bytes_Address of memory_block option * bytes
  | Bytes_MayBytes of indicator * bytes * bytes
  | Bytes_Op of operator * (bytes * Cil.typ) list
  | Bytes_Read of bytes * bytes * int
  | Bytes_Write of bytes * bytes * int * bytes
  | Bytes_FunPtr of Cil.fundec * bytes
and lval_block =
    Lval_Block of memory_block * bytes
  | Lval_May of indicator * lval_block * lval_block
and data_structure = DS_Set of bytes list * bytes
and memory_block_type =
    Block_type_StringLiteral
  | Block_type_Global
  | Block_type_Local
  | Block_type_Heap
and memory_block = {
  memory_block_name : string;
  memory_block_id : int;
  memory_block_size : int;
  memory_block_addr : bytes;
  memory_block_type : memory_block_type;
}
val hash_consing_bytes_hits : int ref
val hash_consing_bytes_misses : int ref
val hash_consing_bytes_init_size : int
val hash_consing_bytes_tbl : (bytes, bytes) Hashtbl.t
val hash_consing_bytes_create : bytes -> bytes
val make_Byte_Concrete : char -> byte
val make_Byte_Symbolic : symbol -> byte
val make_Byte_Bytes : bytes * int -> byte
val make_Bytes_Constant : Cil.constant -> bytes
val make_Bytes_ByteArray : byte ImmutableArray.t -> bytes
val make_Bytes_Address : memory_block option * bytes -> bytes
val make_Bytes_MayBytes : indicator * bytes * bytes -> bytes
val make_Bytes_Op : operator * (bytes * Cil.typ) list -> bytes
val make_Bytes_Read : bytes * bytes * int -> bytes
val make_Bytes_Write : bytes * bytes * int * bytes -> bytes
val make_Bytes_FunPtr : Cil.fundec * bytes -> bytes
val byte__undef : byte
module VarinfoMap :
  sig
    type key = Cil.varinfo
    type 'a t = 'a Cilutility.VarinfoMap.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type memory_frame = { varinfo_to_block : memory_block VarinfoMap.t; }
type operator_action = (bytes * Cil.typ) list -> bytes
module MemoryBlockMap :
  sig
    type key = memory_block
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module VargsMap :
  sig
    type key = bytes
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module LocMap :
  sig
    type key = Cil.location * int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module BytesMap :
  sig
    type key = bytes
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type callingContext =
    Runtime
  | Source of ((lval_block * int) option * Cil.instr * Cil.stmt)
  | NoReturn of Cil.instr
type state = {
  global : memory_frame;
  locals : memory_frame list;
  callstack : Cil.fundec list;
  block_to_bytes : bytes MemoryBlockMap.t;
  path_condition : bytes list;
  path_condition_tracked : bool list;
  callContexts : callingContext list;
  va_arg : bytes list list;
  va_arg_map : bytes list VargsMap.t;
  loc_map : bytes LocMap.t;
  bytes_eval_cache : bool BytesMap.t;
}
val word__size : int
val string_table : bytes MemoryBlockMap.t ref
val stp_count : int ref
module CondSet :
  sig
    type elt = Cil.stmt * bool
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module EdgeSet :
  sig
    type elt = Cil.stmt * Cil.stmt
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module LineSet :
  sig
    type elt = string * int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
type executionHistory = {
  coveredLines : LineSet.t;
  coveredStmts : IntSet.t;
  coveredEdges : EdgeSet.t;
  coveredConds : CondSet.t;
  bytesToVars : (bytes * Cil.varinfo) list;
}
val emptyHistory : executionHistory
module PcHistSet :
  sig
    type elt = bytes list * executionHistory
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val branches_taken :
  (Cil.exp * Cil.location, PcHistSet.t ref * PcHistSet.t ref) Hashtbl.t
module SymbolSet :
  sig
    type elt = symbol
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val signalStringOpt : string option ref
exception SignalException
type job = {
  state : state;
  exHist : executionHistory;
  instrList : Cil.instr list;
  stmt : Cil.stmt;
  inTrackedFn : bool;
  mergePoints : IntSet.t;
  jid : int;
}
type job_result = {
  result_state : state;
  result_history : executionHistory;
}
type job_completion =
    Return of bytes option * job_result
  | Exit of bytes option * job_result
  | Abandoned of string * Cil.location * job_result
  | Truncated of job_result * job_result
type job_state =
    Active of job
  | Fork of job * job
  | Complete of job_completion
module JobSet :
  sig
    type elt = job
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val ifToJoinPointsHash : int Inthash.t
module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
