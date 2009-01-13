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



(* Misc util functions for Proteus *)

open Cil

module H = Hashtbl


(* exception indicating we have reach a state that we shouldn't have *)
exception InternalError of string

(* An internal error indicating work to be done *)
exception FixMe of string




(* List utilities *)

(* return the last n elements of l *)
let list_last_n (l : 'a list) (n : int) : 'a list  = 
   fst (List.fold_right (fun x (acc,n) -> if n=0 then (acc,0) else (x::acc, n-1)) l ([],n))


let hashtbl_size h : int = 
  let n = ref 0 in H.iter (fun _ _ -> incr n) h; !n;;

(* get a list of keys from a hashtbl *)
let hashtbl_keys (h : (('a, 'b) H.t)) : 'a list = 
   H.fold (fun key value accu -> (key::accu)) h [];;

(* get a list of values from a hashtbl *)
let hashtbl_values (h : (('a, 'b) H.t)) : 'b list = 
   H.fold (fun key value accu -> (value::accu)) h [];;

(* convert a hashtbl to a list of val,key pairs *)
let hashtbl_key_val_pairs (h : (('a, 'b) H.t)) : ('a * 'b) list = 
   H.fold (fun key value accu -> ((key,value)::accu)) h [];;

(* add to hashtbl if it's not already there *)
let hashtbl_add_once h k v =
  try 
    ignore(H.find h k)
  with Not_found ->
    H.add h k v

let rec list_last l = match l with 
    [h] -> h
  | h::t-> list_last t
  | [] -> raise (Failure "list_last")

let rec list_chop_last l = match l with 
    [h] -> []
  | h::t-> h::list_chop_last t
  | [] -> raise (Failure "list_chop_last")

let rec list_split n l = 
  if n < 0 
  then raise (Failure ("list_split : negative argument"))
  else match n,l with
      0,_ -> ([],l )
    | n, h::t -> let fh,ft = list_split (n-1) t in
        (h::fh,ft)
    | _, [] -> raise (Failure ("list_split : split pos past list end"))


let rec list_take n l = 
   match n,l with
      0,xs -> []
    | n,x::xs -> x::(list_take (n-1) xs)
    | _ -> raise (Failure "list_take: tried to take too many argumenst!")

let rec list_chop n l = 
   match n,l with 
      0,xs -> xs
    | n,x::xs -> list_chop (n-1) xs
    | _ -> raise (Failure "list_chop: tried to chop too many argumenst!")

(* returns all the elems in l1 that are not in l2 *)
let list_diff l1 l2 cmpFun = 
  List.fold_right (
     fun x accu -> 
		  try 
		     List.find (cmpFun x) l2;
			  accu
		  with 
           Not_found -> x::accu) 
     l1 []

(* Compared to List.(map|iter)2,  these version doesn't throw an exception when lists have different lenghts *)
let rec map2_ignore_trailing f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> let r = f a1 a2 in r :: map2_ignore_trailing  f l1 l2
  | (_, _) -> []

let rec iter2_ignore_trailing f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2_ignore_trailing f l1 l2
  | (_, _) -> ()
 

(* tail recursive version of split *)
let rec list_split2 n l acc = 
  if n < 0 
  then raise (Failure ("list_split : negative argument"))
  else match n,l with
      0,_ -> (List.rev acc,l)
    | n, h::t -> list_split2 (n-1) t (h::acc)
    | _, [] -> raise (Failure ("list_split : split pos past list end"))


(* array_mem copied verbatim from 
   "Introduction to the Objective Caml Programming Language" 
   by Jason Hickey *)
(* determine if x is in array a *)
let array_mem x a = 
  let len = Array.length a in 
  let flag = ref false in 
  let i = ref 0 in 
    while !flag = false && !i < len 
    do 
      if a.(!i) = x 
      then flag := true; incr i 
    done; 
    !flag
