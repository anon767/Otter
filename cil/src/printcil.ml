(** Format-compatible versions of printers in Cil *)

open Pretty
open Cil

let loc = format_adaptor d_loc
let ikind = format_adaptor d_ikind
let fkind = format_adaptor d_fkind
let storage = format_adaptor d_storage
let const = format_adaptor d_const

let unop = format_adaptor d_unop
let binop = format_adaptor d_binop

let exp = format_adaptor d_exp
let lval = format_adaptor d_lval
let offset base = format_adaptor (d_offset base)
let init = format_adaptor d_init
let typ = format_adaptor d_type
let global = format_adaptor d_global
let attrlist = format_adaptor d_attrlist
let attr = format_adaptor d_attr
let attrparam = format_adaptor d_attrparam
let label = format_adaptor d_label
let stmt = format_adaptor d_stmt
let block = format_adaptor d_block
let instr = format_adaptor d_instr
let shortglobal = format_adaptor d_shortglobal

let n_exp = format_adaptor dn_exp
let n_lval = format_adaptor dn_lval
let n_init = format_adaptor dn_init
let n_type = format_adaptor dn_type
let n_global = format_adaptor dn_global
let n_attrlist = format_adaptor dn_attrlist
let n_attr = format_adaptor dn_attr
let n_attrparam = format_adaptor dn_attrparam
let n_stmt = format_adaptor dn_stmt
let n_instr = format_adaptor dn_instr

let plainexp = format_adaptor d_plainexp
let plaintype = format_adaptor d_plaintype
let plaininit = format_adaptor d_plaininit
let plainlval = format_adaptor d_plainlval

let d_exp = format_adaptor dd_exp
let typsig = format_adaptor d_typsig
let formatarg = format_adaptor d_formatarg
