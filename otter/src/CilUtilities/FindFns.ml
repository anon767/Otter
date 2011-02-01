open Cil

(* TODO: This is copied form Coverage.ml. Extract them and put in somewhere. *)
class getCallerVisitor file = object (self)
	val callee_list = ref []
	val currFuncName = ref ""

	method callee_list = !callee_list

	inherit nopCilVisitor

	method vinst instr = (match instr with
      | Call(_, Lval(Var(varinfo), NoOffset) ,_,_) -> (
          match varinfo.vtype with
            | TFun _ ->
                    if not (List.mem varinfo.vname (!callee_list)) then
                        callee_list := (varinfo.vname)::(!callee_list)
                    else ()
            | _ -> ()
        )
      | _ -> ()
    );
		SkipChildren
end

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
let arg_undefined_functions = ref false

let findFns (file : Cil.file) : unit =
	let outChan = if !outFile = "" then stdout else open_out !outFile in
    let all_fnames = get_all_fnames file in
    let all_fnames =
        if !arg_undefined_functions then (
            let vis = new getCallerVisitor file in
            let _ = visitCilFile (vis:>cilVisitor) file in
            let called_fnames = vis#callee_list in
            List.filter (fun fname -> not (List.mem fname all_fnames) &&
                                      not (List.mem ("__otter_libc_"^fname) all_fnames)) called_fnames
        ) else all_fnames
    in
    let all_fnames = List.sort Pervasives.compare all_fnames in
    List.iter (fun fname -> output_string outChan (fname ^ "\n")) all_fnames;
	close_out outChan

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "findFns";
  Cil.fd_description = "Print out the names of all functions in the program.\n";
  Cil.fd_extraopt = [
		("--fnOutfile",
		 Arg.Set_string outFile,
         "<filename> The file to which to write function names (default is stdout)\n");
		("--undefined-functions",
		 Arg.Set arg_undefined_functions,
         " Output undefined functions instead\n");
	];
  Cil.fd_doit = findFns;
  Cil.fd_post_check = false;
}
