open Cil

let outFile = ref ""

let findFns (file : Cil.file) : unit =
	let outChan = if !outFile = "" then stdout else open_out !outFile in
	Cilly.makeCFGFeature.fd_doit file;
	let funs =
		foldGlobals
			file
			(fun funNameList glob ->
				 match glob with
						 GFun(fundec,_) -> fundec.svar.vname :: funNameList
					 | _ -> funNameList)
			[]
	in
	List.iter (fun str -> output_string outChan (str ^ "\n")) funs;
  if outChan != stdout then close_out outChan

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
