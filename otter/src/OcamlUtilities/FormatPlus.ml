(** Useful extensions to the {!Format} module. *)

open Format


 (** Like {!Format.kfprintf}, but does not print anything nor call any %a or %t printers. Useful to ignore some
	material when conditionally printing.
		@deprecated available in {!Format} as of Ocaml version 3.12.
		@see <http://caml.inria.fr/cgi-bin/viewcvs.cgi?rev=9327&view=rev> Ocaml CVS revision r9327 where it was
		introduced.
*)
let ikfprintf k ppf =
	let module Tformat = Printf.CamlinternalPr.Tformat in
	Tformat.kapr (fun _ _ -> Obj.magic (k ppf))


(** Alternative to {!Format.ksprintf} that takes {!Format}-style printers. *)
let ksprintf k format =
	let buffer = Buffer.create 80 in
	let buffer_formatter = Format.formatter_of_buffer buffer in
	Format.kfprintf (fun ff -> pp_print_flush buffer_formatter (); k (Buffer.contents buffer)) buffer_formatter format


(** Alternative to {!Format.sprintf} that takes {!Format}-style printers. *)
let sprintf format = ksprintf (fun s -> s) format


(** Return a string representation of an item given a {!Format}-style printer for the item. *)
let as_string printer item =
	let buffer = Buffer.create 80 in
	let buffer_formatter = Format.formatter_of_buffer buffer in
	printer buffer_formatter item;
	pp_print_flush buffer_formatter ();
	Buffer.contents buffer


(** Alternative to {!Pervasives.failwith} that has a {!Format.printf}-like interface. *)
let failwith format = ksprintf Pervasives.failwith format


(** Print a list of elements, given a printer for the element type and a separator.
		@param printer is the element printer
		@param sep is the separator string, which may include \@ control operators of {!Format.fprintf}
		@return the element list printer
*)
let pp_print_list printer sep = fun ff list ->
	ignore (List.fold_left (fun sep' x -> fprintf ff "%(%)@[%a@]" sep' printer x; sep) "" list)

