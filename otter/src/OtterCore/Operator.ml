(**
    This module should define all functions of type operator_action.
    E.g. 
    let plus (operands: (bytes*typ) list) : bytes =
        ... 
 *)
open DataStructures
open OcamlUtilities
open Cil
open OtterBytes
open Bytes

let unsound_pointer_arithmetic = ref false

let run op operands = op operands


let unop op_conc (*bytearray->bytearray*) op_symb operands : bytes  = 
    let bytes1 = List.nth operands 0 in
    let rec impl bytes =
        match bytes with
            | Bytes_Constant const ->
                impl (constant_to_bytes const)
            | Bytes_ByteArray bytearray when isConcrete_bytearray bytearray ->
                make_Bytes_ByteArray (op_conc bytearray)
            | _ ->
                make_Bytes_Op (op_symb, operands)
    in 
    impl bytes1


let neg operands =
    let op_conc arr =
        (* Negation is flipping the bits and adding 1. To implement this,
             keep track of [carry], the value to add to each byte. We have
             to add 1 to the lowest byte (which is first because we are
             little-endian), and we must continue carrying as long as the
             original byte was '\000'. Once we hit a non-null byte, we stop
             carrying. *)
        let carry = ref 1 in
        ByteArray.map (fun byte -> match byte with
            | Byte_Concrete (c) ->
                let c' = Char.chr ( (!carry + (lnot (Char.code c))) land 255) in
                    if c <> '\000' then carry := 0;
                    make_Byte_Concrete(c')
            | _ -> failwith "neg: unreachable"
        ) arr 
    in
    unop op_conc OP_UMINUS operands 


let bnot operands =
    let op_conc arr =
        ByteArray.map (fun byte -> match byte with
            | Byte_Concrete (c) ->
                let c' = Char.chr ( (lnot (Char.code c)) land 255)  in
                    make_Byte_Concrete(c')
            | _ -> failwith "bnot: unreachable"
        ) arr 
    in
    unop op_conc OP_BNOT operands 

let zero_bytearray = ByteArray.of_list [byte__zero;byte__zero;byte__zero;byte__zero]
let one_bytearray = ByteArray.of_list [byte__make '\001';byte__zero;byte__zero;byte__zero]

let lnot operands = (* should return int (32-bit) *)
    let op_conc arr =
        if ByteArray.fold_left (fun a byte -> match byte with
            | Byte_Concrete (c) -> a || (Char.code c <>0)
            | _ -> failwith "lnot: unreachable"
        ) false arr 
        then (* 0 *)
            zero_bytearray
        else (* 1 *)
            one_bytearray
    in
    unop op_conc OP_LNOT operands 

let typ_to_ikind typ = match unrollType typ with
        TInt (ikind,_) -> ikind
    | TPtr _ -> Lazy.force kindOfUpointType
    | typ -> invalid_arg ("Trying to get ikind from something other than a TInt or TPtr: " ^ (Pretty.sprint 50 (d_type () typ)))


(* binop. suitable for binop with operands of equal type *)
let rec binop op_const op_symb operands optyp : bytes =
    (* TODO: remove optyp; it's only used for making Cil.constants for Bytes.make_Bytes_Constant (which should be removed too) *)
    let bytes1 = List.nth operands 0 in
    let bytes2 = List.nth operands 1 in

    let isReducableArithmetic opout opin = match opout,opin with
      | OP_PLUS,OP_PLUS | OP_PLUS,OP_SUB 
      | OP_SUB,OP_PLUS | OP_SUB,OP_SUB
      | OP_MULT,OP_MULT
      (*| OP_MULT,OP_PLUS | OP_MULT,OP_SUB*)
        -> true
      | _,_ -> false
    in 
    let atLeastOneConstant = List.exists (function Bytes_Constant _ -> true | _ -> false) in
    let plus xs = plus xs optyp in
    let minus xs = minus xs optyp in
    let mult xs = mult xs optyp in
    let rec 
    reducedArithmetic (b1:bytes) (op,args) normal =
        let rab1 = List.nth args 0 in
        let rab2 = List.nth args 1 in
        match op_symb,op with   (* normal: b1 op_symb (rab1 op rab2) , reversed: (rab1 op rab2) op_symb b1  *)
          | OP_PLUS,OP_PLUS -> (* a+(b+c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus [ plus [ b1; rab1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> plus [ plus [ b1; rab2 ]; rab1 ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_PLUS,OP_SUB ->(* a+(b-c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus [ plus [ b1; rab1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> minus [ rab1; minus [ rab2; b1 ] ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_PLUS when normal ->(* a-(b+c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus [ minus [ b1; rab1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> minus [ minus [ b1; rab2 ]; rab1 ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_SUB when normal ->(* a-(b-c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus [ minus [ b1; rab1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> minus [ plus [ b1; rab2 ]; rab1 ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_PLUS ->(* (b+c)-a *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> plus [ minus [ rab1; b1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> plus [ minus [ rab2; b1 ]; rab1 ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_SUB,OP_SUB ->(* (b-c)-a *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> minus [ minus [ rab1; b1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> minus [ rab1; plus [ rab2; b1 ] ]
                  | _,_ -> failwith "unreachable"
              end
          | OP_MULT,OP_MULT ->(* a*(b*c) *)
              begin
                match rab1,rab2 with
                  | Bytes_Constant(_),_ -> mult [ mult [ b1; rab1 ]; rab2 ]
                  | _,Bytes_Constant(_) -> mult [ mult [ b1; rab2 ]; rab1 ]
                  | _,_ -> failwith "unreachable"
              end
          (* dunno if these are helpful *)
          (*| OP_MULT,OP_PLUS ->
              begin
                failwith "not yet implemented"
              end
          | OP_MULT,OP_SUB ->
              begin
                failwith "not yet implemented"
              end*)
          | _ -> failwith "unreachable"
    and
    worstCase b1 b2 = 
            if not (isConcrete_bytes b1 & isConcrete_bytes b2) then
                (make_Bytes_Op(op_symb, operands))
            else
            let c1 = bytes_to_constant b1 optyp in
            let c2 = bytes_to_constant b2 optyp in
            begin match (c1,c2) with
            | (CInt64(i1,k1,s1),CInt64(i2,k2,s2)) ->
                impl (make_Bytes_Constant c1, make_Bytes_Constant c2)
            | (CReal(i1,k1,s1),CReal(i2,k2,s2)) -> (*TMP*) bytes1
            | _ -> failwith "Match error"
            end
    and
    (* Maybe an error of ocaml: if ibytes? is replaced by bytes?, the scoping is messed up *)    
    impl (ibytes1,ibytes2) =
    match (ibytes1, ibytes2) with
        | (Bytes_Constant(CInt64(i1, _, _)), Bytes_Constant(CInt64(i2, _, _))) ->
                (* Some operators always result in ints---namely, relational, equality,
                     and logical operators. For the others, the result's type is that of
                     the first operand,
                     either because both operands have the same type (and the
                     result should have that type, too), or because this is a
                     shift operation.
                     (See 6.3.1.8.1 and much of 6.5 in the Standard.) *)
                let kind = if returnsBoolean op_symb then IInt else typ_to_ikind optyp in
                let n64, _ = Cil.truncateInteger64 kind (op_const i1 i2) in
                let const = CInt64(n64, kind, None) in
                (make_Bytes_Constant(const))
        | (Bytes_Constant(CInt64 _), Bytes_Op (op, args)) when ((isReducableArithmetic op_symb op) && (atLeastOneConstant args))
          -> reducedArithmetic ibytes1 (op,args) true
        | (Bytes_Op (op, args) , Bytes_Constant(CInt64 _)) when ((isReducableArithmetic op_symb op) && (atLeastOneConstant args))
          -> reducedArithmetic ibytes2 (op,args) false
        (* Allow a particular piece of pointer arithmetic: ptr % num. *)
        | Bytes_Address(blk, offset), op2
                when op_symb = OP_MOD &&
                    isConcrete_bytes offset &&
                    isConcrete_bytes op2 ->
                let offsetConstant = bytes_to_constant offset !Cil.upointType in
                let op2Constant = bytes_to_constant op2 !Cil.upointType in
                begin match offsetConstant,op2Constant with
                    | CInt64 _,CInt64 _ ->
                        let ikind = match !Cil.upointType with Cil.TInt (ikind, _) -> ikind | _ -> failwith "Impossible!" in
                        let addr = Cil.CInt64 (Int64.of_int blk.memory_block_addr, ikind, None) in
                        let ptrAsNum = plus [
                            make_Bytes_Constant addr;
                            make_Bytes_Constant offsetConstant
                        ] in
                        impl (ptrAsNum, make_Bytes_Constant op2Constant)
                    | _ -> failwith "Unimplemented pointer arithmetic operation"
                end

        | (b1,b2)  ->
            worstCase b1 b2
    in
        impl (bytes1,bytes2)


and plus operands = binop (fun x y -> Int64.add x y) OP_PLUS operands
and minus operands = binop (fun x y -> Int64.sub x y) OP_SUB operands
and mult operands = binop (fun x y -> Int64.mul x y) OP_MULT operands

(* TODO: fix signed_* for uint64_t; all other cases fit within Int64.t and can be computed correctly *)
let div operands = binop (fun x y -> Int64.div x y) OP_DIV operands
let rem operands = binop (fun x y -> Int64.rem x y) OP_MOD operands
let signed_div operands = binop (fun x y -> Int64.div x y) OP_SDIV operands
let signed_rem operands = binop (fun x y -> Int64.rem x y) OP_SMOD operands

let shiftlt operands = binop (fun x y -> Int64.shift_left x (Int64.to_int y)) OP_LSL operands
let shiftrt operands = binop (fun x y -> Int64.shift_right_logical x (Int64.to_int y)) OP_LSR operands
let signed_shiftrt operands = binop (fun x y -> Int64.shift_right x (Int64.to_int y)) OP_ASR operands

let unsigned_compare x y =
    match (x < 0L, y < 0L) with
        | false, false -> Int64.compare x y
        | true, false -> 1
        | false, true -> -1
        | true, true -> Int64.compare y x

let lt operands = binop (fun x y -> if unsigned_compare x y < 0 then 1L else 0L) OP_LT operands
let gt operands = binop (fun x y -> if unsigned_compare x y > 0 then 1L else 0L) OP_GT operands
let le operands = binop (fun x y -> if unsigned_compare x y <= 0 then 1L else 0L) OP_LE operands
let ge operands = binop (fun x y -> if unsigned_compare x y >= 0 then 1L else 0L) OP_GE operands

let signed_lt operands = binop (fun x y -> if Int64.compare x y < 0 then 1L else 0L) OP_SLT operands
let signed_gt operands = binop (fun x y -> if Int64.compare x y > 0 then 1L else 0L) OP_SGT operands
let signed_le operands = binop (fun x y -> if Int64.compare x y <= 0 then 1L else 0L) OP_SLE operands
let signed_ge operands = binop (fun x y -> if Int64.compare x y >= 0 then 1L else 0L) OP_SGE operands


let eq operands =    
    binop (fun x y -> if x = y then 1L else 0L) OP_EQ operands

let ne operands =    
    binop (fun x y -> if x <> y then 1L else 0L) OP_NE operands

let band operands = binop (fun x y -> Int64.logand x y ) OP_BAND operands
let bxor operands = binop (fun x y -> Int64.logxor x y ) OP_BXOR operands
let bor operands  = binop (fun x y -> Int64.logor x y )  OP_BOR operands

let logand operands =  (* should return int (32-bit) *)
    binop (fun x y -> if x = 0L || y = 0L then 0L else 1L) OP_LAND operands
let logor operands =  (* should return int (32-bit) *)
    binop (fun x y -> if x = 0L && y = 0L then 0L else 1L) OP_LOR operands

let rec opPI op operands typ =
    let bytes1 = List.nth operands 0 in
    let bytes2 = List.nth operands 1 in
    match (bytes1, bytes2) with
        | (Bytes_Address(block, offset), offset2) ->
            begin match unrollType typ with
                | TPtr(basetyp,_) ->
                    let base_size = (Cil.bitsSizeOf basetyp)/8 in
                    let offset3 = mult [ int_to_bytes base_size; offset2 ] !Cil.ptrdiffType in
                    let offset4 = op [ offset ; offset3 ] !Cil.ptrdiffType in
                    (make_Bytes_Address(block, offset4))
                | _ -> failwith "type of Bytes_Address not TPtr"
            end
        | Bytes_ByteArray _, _
        | Bytes_Constant _, _ -> (* Doing pointer arithmetic off of a non-pointer, probably NULL *)
            begin match unrollType typ with
                | TPtr(basetyp,_) ->
                    let base_size = (Cil.bitsSizeOf basetyp)/8 in
                    let offset3 = mult [ bytes2; int_to_bytes base_size ] !Cil.ptrdiffType in
                    op [ bytes1; offset3 ] !Cil.ptrdiffType
                | _ -> failwith "type of Bytes_ByteArray (used as a pointer) not TPtr"
            end
        | Bytes_Conditional c, _ ->
            make_Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [ e; bytes2 ] typ)) c)
        | _, Bytes_Conditional c ->
            make_Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [ bytes1; e ] typ)) c)
        | (Bytes_Read(a, x, l)), _ ->
            let c = (BytesUtility.expand_read_to_conditional a x l) in
            begin match c with
            | Unconditional (Bytes_Read _) -> op [ make_Bytes_Conditional c; bytes2 ] !Cil.ptrdiffType
            | _ -> make_Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [ e; bytes2 ] typ)) c)
            end
        | _, (Bytes_Read(a, x, l)) ->
            let c = (BytesUtility.expand_read_to_conditional a x l) in
            begin match c with
            | Unconditional (Bytes_Read _) -> op [ bytes1; make_Bytes_Conditional c ] !Cil.ptrdiffType
            | _ -> make_Bytes_Conditional (conditional__map (fun e -> conditional__bytes (opPI op [ bytes1; e ] typ)) c)
            end
        | _ ->
            if !unsound_pointer_arithmetic then
                begin
                    Output.set_mode Output.MSG_REPORT;
                    Output.printf "Warning: process opPI unsoundly@\n";
                    Output.printf "@[bytes1:@ @[%a@]@]@." BytesPrinter.bytes bytes1;
                    Output.printf "@[bytes2:@ @[%a@]@]@." BytesPrinter.bytes bytes2;
                    match unrollType typ with
                    | TPtr(basetyp,_) ->
                        let base_size = (Cil.bitsSizeOf basetyp)/8 in
                        let offset = mult [ bytes2; int_to_bytes base_size ] !Cil.ptrdiffType in
                        op [ bytes1; offset ] !Cil.ptrdiffType
                    | _ -> failwith "type of Bytes_Address not TPtr"
                end
            else begin
                Output.set_mode Output.MSG_ERROR;
                Output.printf "@[bytes1:@ @[%a@]@]@." BytesPrinter.bytes bytes1;
                Output.printf "@[bytes2:@ @[%a@]@]@." BytesPrinter.bytes bytes2;
                failwith "plusPI (p1,p2) not of type (addr,int)"
            end


let plusPI operands =
    opPI plus operands


let minusPI operands =
    opPI minus operands

let minusPP operands typ : bytes =
    let bytes1 = List.nth operands 0 in
    let bytes2 = List.nth operands 1 in
    let to_intopt b =
        try Some (Bytes.bytes_to_int_auto b) with Failure _ -> None
    in
    let i1opt = to_intopt bytes1 in
    let i2opt = to_intopt bytes2 in
    match i1opt, i2opt with
    |   Some 0, Some 0 -> 
            (* NULL - NULL *)
            Bytes.bytes__make (Cil.bitsSizeOf typ/8)
    |   _, _ ->
        begin match (bytes1, bytes2) with
        | (Bytes_Address(block1, offset1), Bytes_Address(block2, offset2)) when (Bytes.block__equal block1 block2) ->
            begin match unrollType typ with
                | TPtr(basetyp,_) ->
                    let base_size = (Cil.bitsSizeOf basetyp)/8 in
                    let difference = minus [ offset1; offset2 ] !Cil.ptrdiffType in (* TODO: do we need to cast the offsets? *)
                    div [ difference; int_to_bytes base_size ] !Cil.ptrdiffType
                | _ -> failwith "type of Bytes_Address not TPtr"
            end
        | _ ->
            if !unsound_pointer_arithmetic then
                begin
                    Output.set_mode Output.MSG_REPORT;
                    Output.printf "Warning: process minusPP unsoundly@\n";
                    Output.printf "@[make_Bytes1:@ @[%a@]@]@." BytesPrinter.bytes bytes1;
                    Output.printf "@[make_Bytes2:@ @[%a@]@]@." BytesPrinter.bytes bytes2;
                    match unrollType typ with
                    | TPtr(basetyp,_) ->
                        let base_size = (Cil.bitsSizeOf basetyp)/8 in
                        let difference = minus [ bytes1; bytes2 ] !Cil.ptrdiffType in (* TODO: do we need to cast the offsets? *)
                        div [ difference; int_to_bytes base_size ] !Cil.ptrdiffType
                    | _ -> failwith "type of Bytes_Address not TPtr"
                end
            else begin
                Output.set_mode Output.MSG_ERROR;
                Output.printf "@[make_Bytes1:@ @[%a@]@]@." BytesPrinter.bytes bytes1;
                Output.printf "@[make_Bytes2:@ @[%a@]@]@." BytesPrinter.bytes bytes2;
                failwith "Cannot process minusPP (make_Bytes1,make_Bytes2)"
            end
        end



let options = [
    "--unsound-pointer-arithmetic",
        Arg.Set unsound_pointer_arithmetic,
        " A workaround that enables pointer arithmetic to be carried out using memory_block_addr";
]
