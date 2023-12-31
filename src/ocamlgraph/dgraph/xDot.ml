(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat � l'�nergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

(** Reading XDot files *)

open Graph
open GnomeCanvas
open Dot_ast
open Printf

(* Layout types *)
type pos = float * float      (* coordinates *)
type bounding_box = pos * pos (* bounding box   *)

type node_layout = {
  n_name : string;
  n_pos : pos;                        
  n_bbox   : bounding_box;            
  n_draw   : XDotDraw.operation list;
  n_ldraw  : XDotDraw.operation list;
}

type cluster_layout = {
  c_pos : pos;                        
  c_bbox   : bounding_box;            
  c_draw   : XDotDraw.operation list;
  c_ldraw  : XDotDraw.operation list;
}

type edge_layout = {
  e_draw   : XDotDraw.operation list;
  e_ldraw  : XDotDraw.operation list;
  e_hdraw  : XDotDraw.operation list;
  e_tdraw  : XDotDraw.operation list;
  e_hldraw : XDotDraw.operation list;
  e_tldraw : XDotDraw.operation list;
}

(** Main layout type *)
type ('vertex, 'edge, 'cluster) graph_layout = {
  vertex_layouts  : ('vertex,  node_layout)    Hashtbl.t;
  edge_layouts    : ('edge,    edge_layout)    Hashtbl.t;
  cluster_layouts : ('cluster, cluster_layout) Hashtbl.t;
  bbox : bounding_box;
} 

let mk_node_layout ~name ~pos ~bbox ~draw ~ldraw =
  { n_name    = name;
    n_pos   = pos;
    n_bbox  = bbox;
    n_draw  = draw;
    n_ldraw = ldraw }

let mk_cluster_layout ~pos ~bbox ~draw ~ldraw =
  { c_pos   = pos;
    c_bbox  = bbox;
    c_draw  = draw;
    c_ldraw = ldraw }

let mk_edge_layout ~draw ~ldraw ~hdraw ~tdraw ~hldraw ~tldraw =
  { e_draw   = draw;
    e_ldraw  = ldraw;
    e_hdraw  = hdraw;
    e_tdraw  = tdraw;
    e_hldraw = hldraw;
    e_tldraw = tldraw;
  }

exception ParseError of string

(* MISCELLANEOUS FUNCTIONS *)

let rec take n = function
  | [] -> []
  | l when n = 0 -> []
  | h::t -> h::(take (n-1) t)

let suffix s i = try String.sub s i ((String.length s)-i) 
                 with Invalid_argument("String.sub") -> ""

let rec group_tuples = function
  | [] -> []
  | [x] -> []
  | h1::h2::t -> (h1, h2) :: group_tuples t

(** Splits a string with a separator
   returns a list of strings *)
let split c s = 
  let rec split_from n = 
    try let p = String.index_from s n c 
        in (String.sub s n (p-n)) :: (split_from (p+1)) 
    with Not_found -> [ suffix s n ] 
  in if s="" then [] else split_from 0

(** Converts a coordinate from the dot file to a coordinate on
 the canvas *)
let conv_coord (x,y) =
  let pad = 4. in
  let dot_ppi = 72. in
  let dot_png_ppi = 96. in
  let factor = dot_png_ppi /. dot_ppi in
  (x +. pad) *. factor, -. (y +. pad) *. factor

let read_pos s =
  Scanf.sscanf s "%f,%f" (fun x y -> (x,y))

(** Converts a bounding box of center (x,y), width w and height h
    from a Dot file to a pair of corners (lower left and upper right)
    in the canvas coordinate system

    see http://www.graphviz.org/mywiki/FaqCoordTransformation
    to understand the [pad] and [factor] variables.

    @param pos position of the center of the node, in points.
    @param w width of the node, in inch.
    @param h height of the node, in inch.
*)
let bounding_box (x,y) w h = 
  let dot_ppi = 72. (* number of pixels per inch on a display device *) in
  let dot_png_ppi = 96. (* number of pixels per inch on a display device *) in
  try
    let pad = 4. in
    let x = x +. pad in
    let y = y +. pad in
    let dx = w in
    let dy = h in
    let x1 = x -. dx in
    let y1 = y -. dy in
    let x2 = x +. dx in
    let y2 = y +. dy in
    let factor = dot_png_ppi /. dot_ppi in
    let x1 = x1 *. factor in
    let y1 = -. y1 *. factor in
    let x2 = x2 *. factor in
    let y2 = -. y2 *. factor in
    ((x1,y1),(x2,y2))
  with e -> 
    let s = Printexc.to_string e in
    failwith (Format.sprintf "compute_coord failed : %s@." s)

let get_dot_string = function
  | Dot_ast.String s -> s
  | Dot_ast.Ident s -> s
  | Dot_ast.Number s -> s
  | Dot_ast.Html s -> s

(* READING VERTEX LAYOUTS *)

let read_node_label = function
  | Dot_ast.Ident s
  | Dot_ast.Number s
  | Dot_ast.String s
  | Dot_ast.Html s -> s

let read_points c s = 
  let s' = suffix s (String.index s c) in
  let tokens = List.filter (fun s -> s <> "") (List.tl (split ' ' s')) in
  try match tokens with
    | [] -> None
    | n::t ->
      let n = int_of_string n in
      let floats = List.map float_of_string (take (n*2) t) in
      let points = List.map conv_coord (group_tuples floats) in
      Some points
  with Failure "int_of_string" -> None

(** Finds the attributes [pos], [width] and [height] of a node
    in the attribute list *)
let read_common_layout mk_layout attr_list =
  (* Iter on the attributes *)
  (* shape, position, width, height, color, filled *)
  let fold ((p,w,h, draw,ldraw) as attrs) = function
    | (Dot_ast.Ident "pos"), Some (Dot_ast.String s) ->
        (Some s), w, h, draw,ldraw
    | (Dot_ast.Ident "width"), Some (Dot_ast.String s) ->
        p, (Some s), h, draw,ldraw
    | (Dot_ast.Ident "height"), Some (Dot_ast.String s) ->
        p, w, (Some s), draw,ldraw
    | (Dot_ast.Ident "_draw_"), Some (Dot_ast.String draw) ->
	p,w,h, XDotDraw.parse draw, ldraw
    | (Dot_ast.Ident "_ldraw_"), Some (Dot_ast.String ldraw) ->
	p,w,h, draw, XDotDraw.parse ldraw
    | _ -> attrs in
	
  let fold_attr acc attr_list = 
    List.fold_left fold acc attr_list in
  let attrs = List.fold_left fold_attr (None, None, None, [], [])
    attr_list in

  (* Check if we have position, width and height *)
  match attrs with
    | Some pos, Some w, Some h, draw,ldraw->
	let coord = bounding_box (read_pos pos)
	  (float_of_string w) (float_of_string h) in
	(* Return the node model *)
	let (x,y) = conv_coord (read_pos pos) in
	mk_layout ~pos:(x,y) ~bbox:coord ~draw ~ldraw
    | _,_,_, draw, ldraw ->
	let pos = (0.,0.) in
	let bbox = (0.,0.),(0.,0.) in
	mk_layout ~pos ~bbox ~draw ~ldraw

let read_node_layout (id,_) attrs =
  let f = read_common_layout
    (fun ~pos ~bbox ~draw ~ldraw -> mk_node_layout ~pos ~bbox ~draw ~ldraw)
     attrs in
  f ~name:(get_dot_string id)
let read_cluster_layout = read_common_layout mk_cluster_layout

(* READING EDGE LAYOUTS *)

(** Reads the spline control points of a curve in an xdot file
    example : "c 5 -black B 4 65 296 65 288 65 279 65 270 "
 *)

(* The edge drawing operations are in the following attributes :
   _hdraw_	Head arrowhead
   _tdraw_	Tail arrowhead
   _hldraw_	Head label
   _tldraw_	Tail label
*)

(** Gets the layout of an edge out of the dot ast *)
let read_edge_layout attr_list =
  let draw   = ref [] in
  let ldraw  = ref [] in
  let hdraw  = ref [] in
  let tdraw  = ref [] in
  let hldraw = ref [] in
  let tldraw = ref [] in
  let fill_draw_ops = function
    | (Dot_ast.Ident "_draw_"),   Some (Dot_ast.String s) ->
	draw   := XDotDraw.parse s
    | (Dot_ast.Ident "_ldraw_"),  Some (Dot_ast.String s) ->
	ldraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_hdraw_"),  Some (Dot_ast.String s) ->
	hdraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_tdraw_"),  Some (Dot_ast.String s) ->
	tdraw  := XDotDraw.parse s
    | (Dot_ast.Ident "_hldraw_"), Some (Dot_ast.String s) ->
	hldraw := XDotDraw.parse s
    | (Dot_ast.Ident "_tldraw_"), Some (Dot_ast.String s) ->
	tldraw := XDotDraw.parse s	  
    | _ -> () in
  List.iter (List.iter fill_draw_ops) attr_list;
  let draw, ldraw = !draw, !ldraw in
  let hdraw, tdraw, hldraw, tldraw = !hdraw, !tdraw, !hldraw, !tldraw in
  mk_edge_layout ~draw ~ldraw ~hdraw ~tdraw ~hldraw ~tldraw

(* Computes the bounding box *)
let read_bounding_box str =
  let x1,y1,x2,y2 =
    Scanf.sscanf str "%f,%f,%f,%f"
      (fun a b c d -> a,b,c,d) in

  (* Convert coordinates to the display coordinates *)
  let x1,y1 = conv_coord (x1,y1) in
  let x2,y2 = conv_coord (x2,y2) in
  ((x1,y1), (x2,y2))

module Make
  (G : sig
     include Graph.Sig.G
     val vertex_name : V.t -> string
     val edge_attributes: E.t -> Graph.Graphviz.DotAttributes.edge list
   end) = 
struct

  exception Found of string

  let get_edge_comment e =
    let al = G.edge_attributes e in
    try 
      List.iter (function `Comment c -> raise (Found c) | _ -> ()) al;
      None
    with Found c ->
      Some c

  let get_dot_comment (al : Dot_ast.attr list) =
    try
      List.iter 
	(List.iter
	   (function 
	    | Ident "comment", Some c -> raise (Found (get_dot_string c))
	    | _ -> ()))
      al;
      None
    with Found c ->
      Some c

  let strip_quotes = function
    | "" -> ""
    | s ->
	if s.[0] = '"' && s.[String.length s -1] = '"' then
	  String.sub s 1 (String.length s - 2)
	else s

  (* Parses the graph attribute named id, and converts it with conv *)
  let parse_graph_attr id conv stmts =
    let read_attr = function
      | Ident ident , Some (String attr) when ident = id ->
	  raise (Found attr)
      | _ -> () 
    in
    let read_stmt = function
      | Attr_graph attrs -> List.iter (List.iter read_attr) attrs
      | _ -> () 
    in
    try
      List.iter read_stmt stmts;
      failwith ("Could not find the graph attribute named " ^ id)
    with Found attr ->
      conv attr
    
  let parse_bounding_box = parse_graph_attr "bb" read_bounding_box
  (*let parse_bgcolor = parse_graph_attr "bgcolor" XDotDraw.normalize_color*)

  let parse_layouts g stmts =
    let name_to_vertex = Hashtbl.create 97 in
    let vertices_comment_to_edge = Hashtbl.create 97 in

    let vertex_layouts = Hashtbl.create 97 in
    let edge_layouts = Hashtbl.create 97 in
    let cluster_layouts = Hashtbl.create 97 in

    G.iter_vertex
      (fun v ->
	 let name = strip_quotes (G.vertex_name v) in
	 Hashtbl.add name_to_vertex name v)
      g;

    G.iter_edges_e
      (fun e ->
	 let comment = match get_edge_comment e with
	   | Some c -> Some (strip_quotes c)
	   | None -> None 
	 in
	 let src, dst = G.E.src e, G.E.dst e in
	 Hashtbl.add vertices_comment_to_edge (src,dst,comment) e)
      g;
    
    let find_vertex (id,_) =
      let name = get_dot_string id in
      try Hashtbl.find name_to_vertex name
      with Not_found -> failwith ("Could not find vertex named " ^ name)
    in

    let find_edge v v' comment =
      try Hashtbl.find vertices_comment_to_edge (v,v',comment)
      with Not_found ->
(*	Printf.printf "Did not find edge from %s to %s with comment %s\n"
	  (G.vertex_name v) (G.vertex_name v') 
	  (match comment with Some c -> c | None -> "none");*)
	raise Not_found 
    in
    
    let rec collect_layouts cluster stmt =
      try
	match stmt with 
	| Node_stmt (node_id, al) ->
	    let v = find_vertex node_id in
	    Hashtbl.add vertex_layouts v (read_node_layout node_id al)
	| Edge_stmt (NodeId id, [NodeId id'], al) ->
	    let v  = find_vertex id  in
	    let v' = find_vertex id' in
	    let comment = get_dot_comment al in
	    let e = find_edge v v' comment in
	    Hashtbl.add edge_layouts e (read_edge_layout al)
        | Subgraph (SubgraphDef (Some id, stmts)) ->
	    let cluster = get_dot_string id in
	    List.iter (collect_layouts (Some cluster)) stmts
              (* Anonymous subgraph *)
	| Subgraph (SubgraphDef (_, stmts)) ->
	    List.iter (collect_layouts cluster) stmts
	| Attr_graph al ->
	    (match cluster with
	     | Some c -> Hashtbl.add cluster_layouts c (read_cluster_layout al)
	     | None -> ())
	|  _ -> ()
      with Not_found -> 
	() 
    in
    List.iter (collect_layouts None) stmts;
    vertex_layouts, edge_layouts, cluster_layouts

 let parse g dot_ast =
   let v_layouts, e_layouts, c_layouts = parse_layouts g dot_ast.stmts in
   let bbox = parse_bounding_box dot_ast.stmts in
   (* let bgcolor = parse_bgcolor dot_ast.stmts in*)
   { vertex_layouts  = v_layouts;
     edge_layouts    = e_layouts;
     cluster_layouts = c_layouts;
     bbox = bbox }

 exception DotError of string

 let layout_of_xdot ~xdot_file g =
   let dot_ast = Dot.parse_dot_ast xdot_file in
   parse g dot_ast

 let layout_of_dot ?(cmd="dot") ~dot_file g =
   let base_name = 
     try Filename.basename (Filename.chop_extension dot_file)
     with Invalid_argument _ -> dot_file 
   in
   let xdot_file = Filename.temp_file base_name ".xdot" in
   (* Run graphviz to get xdot file *)
   let dot_cmd = sprintf "%s -Txdot %s > %s" cmd dot_file xdot_file in
   match Sys.command dot_cmd with
   | 0 -> 
       let l = layout_of_xdot ~xdot_file g in
       Sys.remove xdot_file;
       l
   | _ ->
       Sys.remove xdot_file;
       raise (DotError "Error during dot execution") 

end
