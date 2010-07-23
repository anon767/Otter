(** Format-compatible versions of printers in Cil *)

open Pretty
open Cil

let f_loc = format_adaptor d_loc
let f_ikind = format_adaptor d_ikind
let f_fkind = format_adaptor d_fkind
let f_storage = format_adaptor d_storage
let f_const = format_adaptor d_const

let f_unop = format_adaptor d_unop
let f_binop = format_adaptor d_binop

let f_exp = format_adaptor d_exp
let f_lval = format_adaptor d_lval
let f_offset base = format_adaptor (d_offset base)
let f_init = format_adaptor d_init
let f_type = format_adaptor d_type
let f_global = format_adaptor d_global
let f_attrlist = format_adaptor d_attrlist
let f_attr = format_adaptor d_attr
let f_attrparam = format_adaptor d_attrparam
let f_label = format_adaptor d_label
let f_stmt = format_adaptor d_stmt
let f_block = format_adaptor d_block
let f_instr = format_adaptor d_instr
let f_shortglobal = format_adaptor d_shortglobal

let fn_exp = format_adaptor dn_exp
let fn_lval = format_adaptor dn_lval
let fn_init = format_adaptor dn_init
let fn_type = format_adaptor dn_type
let fn_global = format_adaptor dn_global
let fn_attrlist = format_adaptor dn_attrlist
let fn_attr = format_adaptor dn_attr
let fn_attrparam = format_adaptor dn_attrparam
let fn_stmt = format_adaptor dn_stmt
let fn_instr = format_adaptor dn_instr

let f_plainexp = format_adaptor d_plainexp
let f_plaintype = format_adaptor d_plaintype
let f_plaininit = format_adaptor d_plaininit
let f_plainlval = format_adaptor d_plainlval

let fd_exp = format_adaptor dd_exp
let f_typsig = format_adaptor d_typsig
let f_formatarg = format_adaptor d_formatarg
