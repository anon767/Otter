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



(* debug code *)

let abort = Errormsg.s;;
let error = Errormsg.error;;
let bug = Errormsg.bug 

let gprintfNULL (finish : Pretty.doc -> Pretty.doc)  
    (format : ('a, unit, Pretty.doc) format) : 'a =
  let format = (Obj.magic format : string) in

  let flen    = String.length format in
                                        (* Reading a format character *)
  let fget    = String.unsafe_get format in
                                        (* Output a literal sequence of 
                                         * characters, starting at i. The 
                                         * character at i does not need to be 
                                         * checked.  *) 
  let rec literal i = 
    let rec skipChars j = 
      if j >= flen || 
      (match fget j with 
        '%' -> true 
      | '@' -> true 
      | '\n' -> true
      | _ -> false) then
        collect j
      else
        skipChars (succ j)
    in
    skipChars (succ i)
                                        (* the main collection function *)
  and collect (i: int) = 
    if i >= flen then begin
      Obj.magic format
    end else begin
      let c = fget i in
      if c = '%' then begin
        let j = skip_args (succ i) in
        match fget j with
          '%' -> literal j 
        | 's' -> Obj.magic(fun s -> collect (succ j))
        | 'c' ->
            Obj.magic(fun c -> collect (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun n ->
              collect (succ j))
    (* L, l, and n are the Int64, Int32, and Nativeint modifiers to the integer
       formats d,i,o,x,X,u.  For example, %Lo means print an Int64 in octal.*)
        | 'L' -> 
           let j' = succ j in (* eat the d,i,x etc. *)
           Obj.magic(fun n -> collect (succ j'))
        | 'l' ->
            let j' = succ j in (* eat the d,i,x etc. *)
            Obj.magic(fun n -> (succ j'))
        | 'n' ->
            let j' = succ j in (* eat the d,i,x etc. *)
            Obj.magic(fun n -> (succ j'))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(fun f -> collect (succ j))
        | 'b' ->
            Obj.magic(fun b -> collect (succ j))
        | 'a' ->
            Obj.magic(fun pprinter arg -> collect (succ j))
        | 't' ->
            Obj.magic(fun pprinter -> collect (succ j))
        | c ->
            invalid_arg ("dprintf: unknown format %s" ^ String.make 1 c)

      end else if c = '@' then begin
        if i + 1 < flen then begin
          match fget (succ i) with

                
            '['
          | ']'          
          | '!'         
          | '?'        
          | '<' 
          | '>'
          | '^'       
          | '@' -> 
              collect (i + 2)
          | c ->
              invalid_arg ("dprintf: unknown format @" ^ String.make 1 c)
        end else
          invalid_arg "dprintf: incomplete format @"
      end else if c = '\n' then begin
        collect (i + 1)
      end else
        literal i
    end

  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in
  collect 0


(* this shouldPrint is not known statically *)
let log (shouldPrint : bool) (fmt : ('a,unit,Pretty.doc) format) : 'a = 
   if shouldPrint
   then begin
      let f d = Pretty.fprint !Errormsg.logChannel 80 d; flush !Errormsg.logChannel; d in
      Pretty.gprintf f fmt
   end
   else gprintfNULL (fun x->x) fmt
