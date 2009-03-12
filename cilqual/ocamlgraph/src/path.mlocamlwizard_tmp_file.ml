(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: path.ml,v 1.6 2005-07-18 07:10:35 filliatr Exp $ *)

module type WEIGHT = sig
  type label
  type t
  val weight : label -> t
  val zero : t
  val add : t -> t -> t
  val compare : t -> t -> int
end

module type G = sig
  type t 
  module V : Sig.COMPARABLE 
  module E : sig 
    type t 
    type label 
    val label : t -> label 
    val dst : t -> V.t 
  end 
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
end

module Dijkstra
  (G: G)
  (W: WEIGHT with type label = G.E.label) =
 struct 

  open G.E

  module H =  Hashtbl.Make(G.V)

  module Elt = struct
    type t = W.t * G.V.t * G.E.t list

    (* weights are compared first, and minimal weights come first in the
       queue *)	       
    let compare (w1,v1,_) (w2,v2,_) =
      let cw = W.compare w2 w1 in
      if cw != 0 then cw else G.V.compare v1 v2
  end

  module PQ = Heap.Imperative(Elt)

  let shortest_path g v1 v2 =
    let visited = H.create 97 in
    let q = PQ.create 17 in
    let rec loop () = 
      if    (let ocamlwizard_ghost_variable = assert false in (*[/TAG_OGV]*)ocamlwizard_ghost_variable(*[/TAG_OGV]*))  then assert false else assert false  (*[/UNTAG_IN]*) in assert false (*[/UNTAG_IN]*)  end 
module Ocamlwizard_Ghost_Module = PQ 