(** Functions for searching {!Cil.file}. *)


(** Get the memoization tables for searching in a file, initializing them if necessary.
		@param file the {!Cil.file} to get the memoization tables for
		@return [(varinfo_to_fundec, varinfo_to_varinit, name_to_fundec)] a tuple containing the memoization
				tables for searching the file
*)
let globals_memotables =
	let file_memotables = Hashtbl.create 0 in
	fun file ->
		try
			Hashtbl.find file_memotables file
		with Not_found ->
			let varinfo_to_fundec = Hashtbl.create 100 in
			let varinfo_to_varinit = Hashtbl.create 100 in
			let name_to_fundec = Hashtbl.create 100 in
			let name_to_global_varinfo = Hashtbl.create 100 in

			Cil.iterGlobals file begin function
				| Cil.GFun (fundec ,_) ->
					Hashtbl.add varinfo_to_fundec fundec.Cil.svar fundec;
					Hashtbl.add name_to_fundec fundec.Cil.svar.Cil.vname fundec;
				| Cil.GVar (varinfo, initinfo, _) ->
					Hashtbl.add varinfo_to_varinit varinfo initinfo;
					Hashtbl.add name_to_global_varinfo varinfo.Cil.vname varinfo;
				| Cil.GVarDecl (varinfo, _) ->
					Hashtbl.add varinfo_to_varinit varinfo { Cil.init=None };
					Hashtbl.add name_to_global_varinfo varinfo.Cil.vname varinfo;
				| _ -> ()
			end;

			let memotables = object
				method varinfo_to_fundec = Hashtbl.find varinfo_to_fundec
				method varinfo_to_varinit = Hashtbl.find varinfo_to_varinit
				method name_to_fundec = Hashtbl.find name_to_fundec
				method name_to_global_varinfo = Hashtbl.find name_to_global_varinfo
			end in
			Hashtbl.add file_memotables file memotables;
			memotables


(** Find a {!Cil.fundec} by {!Cil.varinfo} from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param varinfo the {!Cil.varinfo} of the {!Cil.fundec} to find
		@return the {!Cil.fundec}
		@raise Not_found if a {!Cil.fundec} for [varinfo] does not exist in [file]
*)
let fundec_by_varinfo file varinfo =
	(globals_memotables file)#varinfo_to_fundec varinfo


(** Find the {!Cil.initinfo} for a {!Cil.varinfo} from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.initinfo} in
		@param varinfo the {!Cil.varinfo} of the {!Cil.initinfo} to find
		@return the {!Cil.initinfo}
		@raise Not_found if a global [varinfo] does not exist in [file]
*)
let global_varinit_by_varinfo file varinfo =
	(globals_memotables file)#varinfo_to_varinit varinfo


(** Find a {!Cil.fundec} by name from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param name the name of the {!Cil.fundec} to find
		@return the {!Cil.fundec}
		@raise Not_found if a {!Cil.fundec} named [name] does not exist in [file]
*)
let fundec_by_name file name =
	(globals_memotables file)#name_to_fundec name


(** Find a global {!Cil.varinfo} by name from a {!Cil.file}.
		@param file the {!Cil.file} to find the {!Cil.fundec} in
		@param name the name of the {!Cil.fundec} to find
		@return the {!Cil.varinfo}
		@raise Not_found if a global {!Cil.varinfo} named [name] does not exist in [file]
*)
let global_varinfo_by_name file name =
	(globals_memotables file)#name_to_global_varinfo name


