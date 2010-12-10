open Cil

let get_all_fnames =
    let all_fnames = ref [] in
    function file ->
        if all_fnames = ref [] then
	        iterGlobals
	        	file
	        	(fun glob ->
	        		 match glob with
	        				 GFun(fundec,_) -> all_fnames := fundec.svar.vname :: (!all_fnames)
	        			 | _ -> ());
        !all_fnames

let outFile = ref ""

let findFns (file : Cil.file) : unit =
	let outChan = if !outFile = "" then stdout else open_out !outFile in
    let all_fnames = get_all_fnames file in
    List.iter (fun fname -> output_string outChan (fname ^ "\n")) all_fnames;
	close_out outChan

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
