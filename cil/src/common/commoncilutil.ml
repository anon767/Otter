(* 
 *
 * Copyright (c) 2004-2006, 
 * 
 *  Iulian Neamtiu      <neamtiu@cs.umd.edu> 
 *  Gareth Stoyle       <gareth.stoyle@cl.cam.ac.uk> 
 *
 * All rights reserved. 
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 *
 * 3. The names of the contributors may not be used to endorse or promote 
 * products derived from this software without specific prior written 
 * permission. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE. 
 *
 *
 *)



(* misc util functions that perform generic operations on CIL structures *)

open Cil

module H = Hashtbl

(* no attributes *)
let noAttr = []
let typeOfEnum = (TInt(IInt,[]))



(* Produce a canonicalised string name for a CIL type *)
let rec canonicalizeFnArgs args isvararg = 
  if args = Some [] && isvararg then "..."
  else
    (if args = None then " "
     else 
       if args = Some [] then "void"
       else 
         let pArg (aname, atype, aattr) accu = 
           accu ^ (cTN atype) ^ ", "
         in
           (List.fold_right pArg  (List.rev (Cil.argsToList args)) "" ) ^
           (if isvararg then "..." else "")
    )

and cTN (t:typ) : string = 
  match t with
    TVoid attr -> "void"
  | TInt (ik, attr) -> 
      let cik = match ik with
          IChar       -> "char"
        | ISChar      -> "signed char"
        | IUChar      -> "unsigned char"
        | IInt        -> "int"
        | IUInt       -> "unsigned int"
        | IShort      -> "short"
        | IUShort     -> "unsigned short"
        | ILong       -> "long"
        | IULong      -> "unsigned long"
        | ILongLong   -> "long long"
        | IULongLong  -> "unsigned long long" 
      in cik
           
  | TFloat (fk, attr) ->        
      let cfk = match fk with
          FFloat      -> "float"
        | FDouble     -> "double"
        | FLongDouble -> "long_double"
      in cfk

  | TComp (comp, attr) -> let su = if comp.cstruct then "struct_" else "union_" in
      (su ^ comp.cname)

  | TEnum (enum, attr) -> 
      ("enum_" ^ enum.ename)

  | TPtr (bt, attr) ->
      let cbt = match bt with 
          TFun(rt, args, isva, fa)  ->
            "(" ^ (cTN rt) ^ ")" ^ " (*) (" ^ (canonicalizeFnArgs args isva) ^ ")"
        | _ -> (cTN bt) ^ " * "
      in cbt

  | TArray (bt, e, attr) -> let cbt = cTN bt in 
      (cbt ^ "_array")

  | TFun (restyp, args, isvararg, attr) ->
      let crt = cTN restyp in
      let cat = canonicalizeFnArgs args isvararg in
        (crt ^ "("  ^ cat ^ ")" )

  | TNamed (bt, attr) -> "typedef_" ^ bt.tname
     
  | TBuiltin_va_list attr -> "__builtin_va_list"



(* Cleans up type canonicalised type name *)
let sanitizeTypeName s = 
  Str.global_replace (Str.regexp "[ ]") "_" (
    Str.global_replace (Str.regexp "[,]") "_C_" ( 
      Str.global_replace (Str.regexp "[(]") "_L_" (
        Str.global_replace (Str.regexp "[)]") "_R_" (
          Str.global_replace (Str.regexp "[*]") "PTR" (s)))))


(* Create a statement making sure the statement gets a unique sid *)
let mkStmt s = 
   let newS = Cil.mkStmt s in 
   newS.sid <- !Cil.sid_counter;
   incr Cil.sid_counter;
   newS


(* EXPLAINME: d_plainoffset in CIL is bad for some reason....? *)
let rec my_d_plainoffset () off = match off with
    NoOffset -> Pretty.text "NoOffset"
  | Field(fi,o) -> Pretty.dprintf "Field(@[%s:%a,@?%a@])" fi.fname  d_plaintype fi.ftype my_d_plainoffset o
  | Index(e, o) -> Pretty.dprintf "Index(@[%a,@?%a@])" d_plainexp e my_d_plainoffset o




(* Given a list of fun args, returns a list of their types *)
let argsToTypesList = function
    None -> []
  | Some al -> List.map (fun (x,y,z) -> y) al



let rec d_plainoffset () off = match off with
    NoOffset -> Pretty.text "NoOffset"
  | Field(fi,o) -> Pretty.dprintf "Field(@[%s:%a,@?%a@])" fi.fname  d_plaintype fi.ftype d_plainoffset o
  | Index(e, o) -> Pretty.dprintf "Index(@[%a,@?%a@])" d_plainexp e d_plainoffset o



let stripTypeAttributes t = 
(* Keep track of comps we have seen in order to stop recursion *)
   let compHash = H.create 7 in 
   let rec stripTypeAttributes' t = 
     match t with
       | TPtr(targ,al) -> TPtr(stripTypeAttributes' targ, noAttr)
       | TArray(targ,e,al) -> TArray(stripTypeAttributes' targ,e,noAttr)
   
       | TFun(rt,args,isva,al) -> 
          TFun(stripTypeAttributes' rt,
                (match args with
                   None -> None
                 | Some l -> 
                   let newL = 
                      (List.map (fun (x,y,z) -> 
                                   ("", stripTypeAttributes' y, z)) l) in
                                             Some newL), isva, noAttr)
   
       | TVoid(al) -> TVoid(noAttr)
       | TBuiltin_va_list (al) -> TBuiltin_va_list (noAttr)
       | TInt(ik,al) -> TInt(ik, noAttr) 
       | TFloat(fk,al) ->TFloat(fk, noAttr)
       | TEnum(ei,al) -> TEnum(ei,noAttr)
       | TComp(ci,al) -> t 
           (*if H.mem compHash ci.cname then TComp(ci,noAttr)
           else begin
              H.add compHash ci.cname true;
              let stripFieldAttr fi =
                 fi.fattr <- noAttr;
                 fi.ftype <- stripTypeAttributes' fi.ftype;
                 fi
              in 
              ci.cfields <- List.map stripFieldAttr ci.cfields;
              TComp(ci,noAttr)
           end*)
       | TNamed(ti,al) -> t
           (* let newTi =
             begin
               let ti' = ti in
                 begin
                   ti'.ttype <- stripTypeAttributes' ti.ttype;
                   ti'
                 end
             end
           in
             TNamed(newTi, noAttr)*)
     in
     let t' = stripTypeAttributes' t in 
     t'


(* Given a struct t and a label name name, construct a field info.
   We use this to access fields in structs *)
let getFieldinfoByName t name loc_opt = 
  match t with
      TComp(ci,attr) -> 
         begin try
           Cil.getCompField ci name
         with Not_found -> 
           Commondebug.abort (Commondebug.bug "getFieldinfoByName : type %a doesn't have a field named %s !\n" d_plaintype t name)
         end  
    | _ -> 
      begin match loc_opt with 
         None ->
          Commondebug.abort (Commondebug.error 
             "getFieldinfoByName type (%a) is not a struct/union !\n" 
             d_plaintype t)
       | Some loc -> 
          Commondebug.abort (Commondebug.error 
             "(%a) getFieldinfoByName type (%a) is not a struct/union !\n" 
             d_plaintype t d_loc loc)
      end



(* get canonical name for a CABS global *)
let getCABSGlobalName (g : Cabs.definition) : string = 
   let extractName (n, _, _, _) = n in
   match g with

        Cabs.TYPEDEF (names, loc) -> 
           let typeName = extractName (List.hd (snd names)) in 
           "typedef \"" ^ typeName ^ "\"" 

      | Cabs.ONLYTYPEDEF ([Cabs.SpecType(Cabs.Tstruct(typeName, _, _))], loc) -> 
           "struct \"" ^ typeName ^ "\""

      | Cabs.ONLYTYPEDEF ([Cabs.SpecType(Cabs.Tunion (typeName, _, _))], loc) ->
           "union \"" ^ typeName ^ "\""

      | Cabs.ONLYTYPEDEF ([Cabs.SpecType(Cabs.Tenum  (typeName, _, _))], loc) ->
           "enum \"" ^ typeName ^ "\""

      | Cabs.FUNDEF (proto, body, loc, loc') ->
           let functionName = extractName (snd proto) in
           "function \"" ^ functionName ^ "\""

      | _ -> 
           Cprint.print_def g;
           Cprint.flush ();
           Commondebug.abort (Commondebug.bug "getCABSGlobalName : tried to get canonical name for this unforseen global!\n")

(* we don't have to pass the whole global for these, so that's convenient
   if we don't have the whole global.
*)

let getGlobalNameLight_gtype name = "typedef \"" ^ name ^ "\""
let getGlobalNameLight_gvardecl name = "decl " ^ "var \"" ^ name ^ "\""
let getGlobalNameLight_gvar name = "var \"" ^ name ^ "\""
let getGlobalNameLight_gfun name = "function \"" ^ name ^ "\""

(* get canonical name for a CIL global *)
let rec getGlobalName (g : Cil.global) : string = match g with
    GType (t, l) -> getGlobalNameLight_gtype t.tname
  | GCompTag (c, l) -> if c.cstruct then "struct \"" ^ c.cname ^ "\"" else "union \"" ^ c.cname ^ "\"" 
  | GCompTagDecl (c, l) -> "decl " ^ (getGlobalName (GCompTag (c, l)))
  | GEnumTag (e, l) -> "enum \"" ^ e.ename ^ "\""
  | GEnumTagDecl (e, l) -> "decl " ^ (getGlobalName (GEnumTag (e, l)))
  | GVarDecl (v, l) -> getGlobalNameLight_gvardecl v.vname
  | GVar  (v, i, l) -> getGlobalNameLight_gvar v.vname
  | GFun (f, l) -> getGlobalNameLight_gfun f.svar.vname
  | GAsm (s, l) -> "asm"
  | GPragma (a, l) -> "pragma"
  | GText (s) -> "text"

(* get canonical name for a global 
   strive to be non-clashing, e.g. can be used as key in a hash
 *)
let rec getGlobalNameNonClashing (g : Cil.global) : string = 
   let stringified_global = match g with
        GType (t, l) -> ("typedef \"" ^ t.tname ^ "\"") ,l
      | GCompTag (c, l) -> (if c.cstruct then "struct \"" ^ c.cname ^ "\"" else "union \"" ^ c.cname ^ "\""), l
      | GCompTagDecl (c, l) -> ("decl " ^ (getGlobalName (GCompTag (c, l)))), l
      | GEnumTag (e, l) -> ("enum \"" ^ e.ename ^ "\""), l
      | GEnumTagDecl (e, l) -> ("decl " ^ (getGlobalName (GEnumTag (e, l)))), l
      | GVarDecl (v, l) -> ("decl " ^ "var \"" ^ v.vname ^ "\""), l
      | GVar  (v, i, l) -> ("var \"" ^ v.vname ^ "\""), l
      | GFun (f, l) -> ("function \"" ^ f.svar.vname ^ "\""), l
      | GAsm (s, l) -> ("asm " ^ (string_of_int (Hashtbl.hash s))), l
      | GPragma (a, l) -> ("pragma" ^ (string_of_int (Hashtbl.hash a))), l
      | GText (s) -> ("text " ^ (string_of_int (Hashtbl.hash s))), Cil.locUnknown
   in
   let l = snd stringified_global in
      (fst stringified_global) ^ "@" ^ (l.file) ^ ":" ^ (string_of_int l.byte)

(* extract the name of a global based on its canonical (string) name returned by getGlobalName *)
let extractGlobalName (s : string) : string = 
   try
      let firstQuoteIndex = String.index s '"' in
      let len = String.length s in
      String.sub s (firstQuoteIndex + 1) (len - firstQuoteIndex - 2)
   with _ ->
      Commondebug.abort (Commondebug.bug "extractGlobalName : malformed name '%s'!\n" s)
 

(* test whether this is a builtin function *)
let isBuiltin (fname : string) : bool = 
  H.mem (if !Cil.msvcMode then Cil.msvcBuiltins else Cil.gccBuiltins) fname

(* test whether this type is a CIL-generated anonymous type *)
let isAnonymous (typeName : string) : bool = Cil.startsWith "__anon" typeName

(* return the number of args a function takes *)
let getFunctionArgCount t = match t with 
     TFun(_, Some l, _, _) -> List.length l
   | TFun(_, None, _, _) -> 0
   | _ -> Commondebug.abort (Commondebug.bug " getFunctionArgCount: called on non-function type %a\n" 
                                d_plaintype t)

(* Check whether this is TFun or TPtr to some TFun *)
let isFunctionOrPointerToFunctionType t = Cil.isFunctionType (Cil.unrollTypeDeep t)

let rec prettyPrintType ?(effect_to_string = (fun x -> "")) t = 
(* effect_to_string is optional beacuse we only use it when
   we call prettyPrintType from Updatedebug.ppType 
   Other call sites should be oblivious of this trick
*)
   match t with
      TVoid _ -> "void"
    | TInt (ik, attr) -> ( match ik with
           IChar       -> "char"
         | ISChar      -> "signed char"
         | IUChar      -> "unsigned char"
         | IInt        -> "int"
         | IUInt       -> "unsigned int"
         | IShort      -> "short"
         | IUShort     -> "unsigned short"
         | ILong       -> "long"
         | IULong      -> "unsigned long"
         | ILongLong   -> "long long"
         | IULongLong  -> "unsigned long long" )

    | TFloat (fk, attr) -> ( match fk with
           FFloat      -> "float"
         | FDouble     -> "double"
         | FLongDouble -> "long double" )

 
    | TPtr (t',_) -> "(" ^ (prettyPrintType t') ^ ")" ^ "*" 
    | TArray (t',_,_) -> (prettyPrintType t') ^ "[]"
    | TFun (t_res,t_args_opt,isva,attrs) ->
        (* if the attributes contain any effects, include the EFFECT keyword on the arrow*)
        let theEffects = effect_to_string attrs in
        let rec printTheArgs acc l =
               match l with 
                   [] -> acc
                 | (_,t_arg,_)::xs -> printTheArgs (acc ^ (prettyPrintType t_arg) ^ " -> ") xs
        in 
        let al = Cil.argsToList t_args_opt in
          (* if al is empty, print a void as the arg type *)
        let theArgs = match al with
            [] -> "() -> "
          | _ -> printTheArgs "" al
        in
        let theVarArgs = if isva then "..." else "" 
        in
          (theArgs) ^
          (theVarArgs) ^
          (theEffects) ^
          (prettyPrintType t_res)
       
    | TNamed (ti,_) -> ti.tname
    | TComp (ci,_) ->
       let compKind = 
          if ci.cstruct 
          then "struct"
          else "union"
       in 
       compKind ^ " " ^ ci.cname
       
    | TEnum (ei,_) -> "enum " ^ ei.ename
    
    | TBuiltin_va_list _ -> "__built_in_valist"
      
(* make a field in struct fci called fname with type ftype *)
let makeField (fci : Cil.compinfo) (fname : string) (ftype : Cil.typ) : Cil.fieldinfo = 
   {
      fcomp = fci;
      fname = fname;
      ftype = ftype;
      fbitfield = None;
      fattr = [];
      floc = Cil.locUnknown;
   } 

      
(* make a compinfo called name. isStruct is false for union, true fro struct *)
let makeCompInfo (isStruct : bool) (name : string) : Cil.compinfo = 
   let ci = 
      {
         cstruct = isStruct;
         cname = name;
         ckey = !Cil.nextCompinfoKey; 
         cfields = [];
         cattr = [];
         cdefined = true;
         creferenced = false;
      }
   in
      incr Cil.nextCompinfoKey;
      ci

(*-----------------*)
let isNamedType t = 
(*-----------------*)
   match t with 
      TNamed _ -> true
    | _        -> false

(* Shallow comparison for type definitions:
   
   TInt's and TFloat's are compared by kinds.
   TNamed/TComp/TEnum's are only compared by name.

   Patchcomp and indirection use this to test whether a fucntion has changed
   signatures
*)
let rec isEqualTypeQuick ?(comparator = (fun n1 n2 ty1 ty2 -> n1 = n2)) (t1 : Cil.typ) (t2 : Cil.typ) : bool = begin match t1,t2 with

     TVoid _, TVoid _ -> true

   | TInt (ik1,_), TInt(ik2,_) -> ik1 = ik2  
   | TFloat (fk1,_), TFloat(fk2,_) -> fk1 = fk2
                                                                   
   | TPtr(t1', _), TPtr(t2', _) -> isEqualTypeQuick  ~comparator:comparator t1' t2'
   | TArray(t1', _, _), TArray(t2', _, _) -> isEqualTypeQuick  ~comparator:comparator t1' t2'

   | (TFun _ as t1'), (TFun _ as t2') -> isEqualFuntypeQuick  ~comparator:comparator t1' t2' 

   | TNamed(ti1, _), TNamed(ti2, _) -> 
        comparator ti1.tname ti2.tname t1 t2

   | TComp(ci1, _), TComp(ci2, _) -> 
        ci1.cstruct = ci2.cstruct && 
        comparator ci1.cname ci2.cname t1 t2

   | TEnum(ei1, _), TEnum(ei2, _) ->
        comparator ei1.ename ei2.ename t1 t2

   | TBuiltin_va_list _, TBuiltin_va_list _ -> true

   | _, _ -> false
end
                                                                   
(* check function signature for changes *)   
and isEqualFuntypeQuick ?(comparator = (fun n1 n2 ty1 ty2 -> n1 = n2)) (t1 : Cil.typ) (t2 : Cil.typ) : bool = begin match t1,t2 with
      (* the 'no arguments' case *) 
     TFun(tRes1, None, isva1, _), TFun(tRes2, None, isva2, _) ->
         isva1 = isva2 &&
         isEqualTypeQuick  ~comparator:comparator tRes1 tRes2

   (* the 'some arguments' case: arguments should have the same types *)
   | TFun(tRes1, Some l1, isva1, _), TFun(tRes2, Some l2, isva2, _) ->
        isva1 = isva2 &&
        isEqualTypeQuick  ~comparator:comparator tRes1 tRes2 &&
        (try 
            List.for_all2 (fun (_, argType1, _) (_, argType2, _) -> isEqualTypeQuick ~comparator:comparator argType1 argType2) l1 l2 
         with
            (* arg lists don't have the same length; then it has changed signature for sure *)
             _ -> false
        )

   (* gross mismatch *)
   | _ -> false
end;;

let isStructType t = match Cil.unrollType t with
     TComp _ -> true
   | _ -> false;;


let rec getBaseVarinfoLval (lv : Cil.lval) : Cil.varinfo = match lv with 
     Var v, off -> v
   | Mem e, off -> getBaseVarinfo e

and getBaseVarinfo (e : Cil.exp) : Cil.varinfo = match e with

     Lval lv  | AddrOf lv | StartOf lv -> 
        getBaseVarinfoLval lv

   | _ -> 
        Commondebug.abort (Commondebug.bug "getBaseVarinfo expression %a doesn't reduce to a var!\n" d_plainexp e);;

(* return all global variables used in current expr/stmt/instr/etc. *)
class collectGlobalVarsVisitor hash : cilVisitor = object(self)
   inherit nopCilVisitor
      
   method vvrbl v = 
      (if v.vglob 
      then Commonutil.hashtbl_add_once hash v.vname ());
      SkipChildren
end;;


let getGlobalVarsUsedInInstr i = 
   let res = H.create 37 in
   let () = ignore (Cil.visitCilInstr (new collectGlobalVarsVisitor res) i) in
   Commonutil.hashtbl_keys res;;
