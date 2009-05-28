open Cil

let outFile = ref ""

let findFns (file : Cil.file) : unit =
	let outChan = if !outFile = "" then stdout else open_out !outFile in
	iterGlobals
		file
		(fun glob ->
			 match glob with
					 GFun(fundec,_) -> output_string outChan (fundec.svar.vname ^ "\n")
				 | _ -> ())

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "findFns";
  Cil.fd_description = "Print out the names of all functions in the program.\n";
  Cil.fd_extraopt = [
		("--fnOutfile",
		 Arg.Set_string outFile,
		 "<filename> The file to which to write function names (default is stdout)\n")
	];
  Cil.fd_doit = findFns;
  Cil.fd_post_check = false;
}
