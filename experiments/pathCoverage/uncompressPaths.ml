let main filename =
  let inChan = open_in filename in
  try
    while true do
			let line = ref (input_line inChan) in
      while !line <> "these 1 paths are hit" do
				print_endline !line;
				line := input_line inChan;
			done;
			print_endline !line; (* Print "these 1..." *)
			line := Uuencode.uudecode (input_line inChan); (* uudecode the path to its bzipped form *)
			let uncompressed = Bz2.uncompress !line 0 (String.length !line) in (* unbzip it *)
			try (* Make it into one long line by replacing '\n's with ','s *)
				(* Now, the paths have commas, so this is unnecessary. But vsftpd's
					 data was generated with newlines, so I'm leaving this here. *)
				let rec replaceAllNewlinesFrom idx =
					let idx' = String.index_from uncompressed idx '\n' in
					uncompressed.[idx'] <- ','; replaceAllNewlinesFrom idx';
					replaceAllNewlinesFrom idx'
				in replaceAllNewlinesFrom 0
			with Not_found ->
				print_endline uncompressed; (* And print the string out *)
    done;
  with End_of_file ->
    ()
;;

Arg.parse [] main "Give me a filename, and I'll unfold the paths (and print the results to stdout)"
;;
