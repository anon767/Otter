open OcamlUtilities

module StringSet = Set.Make(String)

let arg_remove_cil_suffixes = ref true
let arg_tracked_fns = ref None
let arg_untracked_fns = ref None

let rec remove_cil_suffix =
    let pat = Str.regexp "___[0-9]+$" in
    function str ->
    (* Remove potential ___n suffix of str *) 
        try
            let index = Str.search_forward pat str 0 in
            let str' = String.sub str 0 index in
            Format.printf "Tracking %s as though it were %s@\n" str str';
            remove_cil_suffix str'
        with Not_found -> str

let isTracked fname trackedFns = 
    let fname = 
        if !arg_remove_cil_suffixes then
            remove_cil_suffix fname 
        else fname
    in
    StringSet.mem fname trackedFns

let trackedFns =
    let module Memo = Memo.Make (CilUtilities.CilData.CilFile) in
    Memo.memo "TrackingFunctions.trackedFns" begin fun file ->
        match !arg_tracked_fns, !arg_untracked_fns with
            | None, None -> List.fold_left (fun set elt -> StringSet.add elt set) StringSet.empty (CilUtilities.FindFns.get_all_fnames file)
            | None, Some fns -> List.fold_left (fun set elt ->
                    if List.mem (remove_cil_suffix elt) fns then set
                    else StringSet.add elt set
                ) StringSet.empty (CilUtilities.FindFns.get_all_fnames file)
            | Some fns, None -> List.fold_left (fun set elt -> StringSet.add elt set) StringSet.empty fns
            (* FIXME: we can allow both lists to be active, e.g., tracedFns = arg_tracked_fns \ arg_untracked_fns *)
            | Some _, Some _ -> failwith "whitelist and blacklist both active"
    end

let options = [
	("--tracked-functions",
		Arg.String begin fun filename ->
			let inChan = open_in filename in
			try
				while true do
					match !arg_tracked_fns with
						| None -> arg_tracked_fns := Some [input_line inChan]
						| Some fns -> arg_tracked_fns := Some ((input_line inChan)::fns)
				done
			with End_of_file ->
				close_in inChan;
				arg_untracked_fns := None
		end,
		"<filename> File containing a list of functions to track. Not compatable with --untracked-functions. Default is to track all functions.\n"
	);
	("--untracked-functions",
		Arg.String begin fun filename ->
			let inChan = open_in filename in
			try
				while true do
					match !arg_untracked_fns with
						| None -> arg_untracked_fns := Some [input_line inChan]
						| Some fns -> arg_untracked_fns := Some ((input_line inChan)::fns)
				done
			with End_of_file ->
				close_in inChan;
				arg_tracked_fns := None
		end,
        "<filename> File containing a list of functions whose coverages are not tracked. Not compatable with --tracked-functions. Default is to track all functions.\n");
	("--no-remove-cil-suffixes",
		Arg.Clear arg_remove_cil_suffixes,
		" Disable removal of cil suffixes (___0) during processing tracked functions\n");
]
