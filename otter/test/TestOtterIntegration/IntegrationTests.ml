open TestUtil.MyOUnit
open TestUtil.OtterUtil
open Otter


(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterIntegration"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration main_loop dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun path ->
        let fullpath = Filename.concat fulldir path in

        path >:: test_otter_on_file fullpath ~main_loop begin fun file results ->
            (* count jobs that were abandoned *)
            let abandoned = List.fold_left begin fun abandoned result -> match result with
                | Types.Abandoned (s, loc, _) -> (loc.Cil.file, loc.Cil.line, s)::abandoned
                | _ -> abandoned
            end [] results in
            if abandoned <> [] then
                let printer = list_printer (fun ff (f, l, s) -> Format.fprintf ff "@[%s:%d: %s@]" f l s) "@\n" in
                assert_failure "@[<hv2>Abandoned paths:@\n%a@]" printer abandoned
        end

    end


(*
 * OUnit test suite
 *)

let testsuite = "Integration" >::: [
	test_integration Driver.init "OtterCore";
	test_integration Multiprocess.init "MultiprocessOtter";
]

