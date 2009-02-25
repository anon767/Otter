open Types
open MemOp
open Cil

type ds_op = (state -> (Types.MemoryBlockMap.key * Types.bytes * int) option -> Cil.exp list -> state);;

 (*

let aux_check_pars fun_name exps num =
  if (List.length exps <> num) then failwith (fun_name ^ "num of pars <> "^(string_of_int num))
  else ()
;;

let op__SET_INIT state blkOffSizeOpt exps =
  (* pars: set pointer, rest object *)
  aux_check_pars "op__SET_INIT" exps 2;
  let target_exp = List.nth exps 0 in
  match target_exp with
    | Lval (target_lval) ->
      let (target_lval_block,target_lval_offset) = Eval.lval state target_lval in
      let rest_exp = List.nth exps 1 in
      let rest_bytes = Eval.rval state rest_exp in
      let all_bytes = Bytes_DS ( DS_Set ([],rest_bytes) ) in
      let all_block = block__make "@SET" (-1) Block_type_Heap in
      let state2 = state__add_block state all_block all_bytes in
      let address = Bytes_Address(Some(all_block),bytes__zero) in
      let state3 = state__assign state2 (target_lval_block, target_lval_offset, word__size) address in
        state3
    | _ -> failwith "op__SET_INIT first par not an lval"                                     
;;

let op__SET_FIND state blkOffSizeOpt exps =
  (* pars: variable, set pointer, constraint *)
  aux_check_pars "op__SET_FIND" exps 3;
(*

  let target_exp = List.nth exps 0 in
  match target_exp with
    | Lval (target_lval) ->
      let (target_lval_block,target_lval_offset) = Eval.lval state target_lval in
      let rest_exp = List.nth exps 1 in
      let rest_bytes = Eval.rval state rest_exp in
      let all_bytes = Bytes_DS ( DS_Set ([],rest_bytes) ) in
      let all_block = block__make "@SET" (-1) Block_type_Heap in
      let state2 = state__add_block state all_block all_bytes in
      let address = Bytes_Address(Some(all_block),bytes__zero) in
      let state3 = state__assign state2 (target_lval_block, target_lval_offset, word__size) address in
        state3
    | _ -> failwith "op__SET_INIT first par not an lval"                                     
 *) 

  state
;;

  *)
