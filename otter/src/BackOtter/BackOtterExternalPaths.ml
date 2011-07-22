open OtterCore

let filenames = ref []

let get_path file lines =
    let rec impl = function
        | [] -> DecisionPath.empty
        | line :: lines ->
            let decision = Decision.from_string file line in
            let decision_path = impl lines in
            DecisionPath.add decision decision_path
    in
    impl lines


let init file = List.iter begin
        fun filename ->
            let lines = ref [] in
            let inChan = open_in filename in
            begin try
                while true do
                    lines := (input_line inChan) :: (!lines)
                done
            with End_of_file ->
                close_in inChan
            end;
            let lines = List.rev (!lines) in
            let fname = List.hd lines in
            let fundec = CilUtilities.FindCil.fundec_by_name file fname in
            let path = get_path file (List.tl lines) in
            ignore (BackOtterTargets.add_path fundec path)
    end (!filenames)

let options = [
    "--backotter-external-path",
        Arg.String begin fun str -> filenames := str :: (!filenames) end,
        "<filename> File containing one external path";
]
