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

(** View classes.

    Each optional function [delay_node], [delay_edge] and [delay_cluster] of
    this module may be used to indicate whether an element must be displayed
    instantaneously (if the function returns [false]) or may be delayed for
    latter display (if the function returns [true]). By default, each function
    always returns [false]. It may be set for returning [true] from time to
    time, improving efficiency. *)

open DGraphViewItem

(** Graph widget derived from [GnoCanvas.canvas].
    Support zooming and scrolling. *)
class ['vertex, 'edge, 'cluster] view:
  ?delay_node:('vertex -> bool) ->
  ?delay_edge:('edge -> bool) ->
  ?delay_cluster:('cluster -> bool) ->
  GnomeCanvas.canvas Gtk.obj ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
object
  inherit GnoCanvas.canvas

  method model : ('vertex, 'edge, 'cluster) DGraphModel.abstract_model

  (** {2 Getters} *)

  method get_node : 'vertex -> 'vertex view_item
  method get_edge : 'edge -> 'edge view_item
  method get_cluster : 'cluster -> 'cluster view_item

  (** {2 Iterators} *)

  method iter_nodes:  ('vertex view_item -> unit) -> unit
  method iter_edges: ('vertex view_item -> 'vertex view_item -> unit) -> unit
  method iter_edges_e:  ('edge view_item -> unit) -> unit
  method iter_clusters: ('cluster view_item -> unit) -> unit

  method iter_succ: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred: ('vertex view_item -> unit) -> 'vertex view_item -> unit
  method iter_succ_e: ('edge view_item -> unit) -> 'vertex view_item -> unit
  method iter_pred_e: ('edge view_item -> unit) -> 'vertex view_item -> unit

  (** {2 Membership functions} *)

  method mem_edge: 'vertex view_item -> 'vertex view_item -> bool
  method find_edge: 'vertex view_item -> 'vertex view_item -> 'edge view_item
  method src: 'edge view_item -> 'vertex view_item
  method dst: 'edge view_item -> 'vertex view_item

  (** {2 Zooming} *)

  method zoom_factor : float

  method zoom_to : float -> unit
  method zoom_in : unit -> unit
  method zoom_out : unit -> unit
  method adapt_zoom : unit -> unit

  method set_zoom_padding: float -> unit
    (** Set the zoom padding used by [zoom_in] and [zoom_out]. *)

  (** {2 Highlighting} *)

  method connect_highlighting_event: unit -> unit

  method highlight: ?color:string * string -> 'vertex view_item -> unit
    (** Change the color of the given vertex item.
	May be cancelled by [dehighlight].
	If [color] is [primary,secondary], then
	[primary] is used except if the current color is [primary]. In this
	case, [secondary] is used. *)

  method dehighlight: 'vertex view_item -> unit
    (** Cancel [highlight]. *)

end

val view:
  ?aa:bool (** Anti-aliasing *) ->
  ?delay_node:('vertex -> bool) ->
  ?delay_edge:('edge -> bool) ->
  ?delay_cluster:('cluster -> bool) ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  ('vertex, 'edge, 'cluster) view
  (** View as a Gnome Canvas.
      Support zooming and scrolling. *)

(* Same widget augmented with highlighting, focus
    and the ability to drag the canvas (click'n hold)
*)
(* class ['vertex, 'edge, 'cluster] drag_view : *)
(*   GnomeCanvas.canvas Gtk.obj -> *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ['vertex, 'edge, 'cluster] view *)

(* val drag_view : *)
(*   ?aa:bool -> (\** Anti aliasing *\) *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ?border_width:int -> *)
(*   ?width:int -> *)
(*   ?height:int -> *)
(*   ?packing:(GObj.widget -> unit) -> *)
(*   ?show:bool -> unit *)
(*   -> ('vertex, 'edge, 'cluster) highlight_focus_view *)
