open TestUtil.MyOUnit
open TestUtil.OtterUtil
open Otter


(* directory containing tests for IntegrationTests *)
let dir_prefix = Filename.concat "test" "TestOtterSystem"


(* test helper that runs the symbolic executor on all files relative to dir_prefix/dir *)
let test_integration main_loop dir =
    let fulldir = Filename.concat dir_prefix dir in

    dir >: test_dir fulldir begin fun path ->
        let fullpath = Filename.concat fulldir path in

        path >:: test_with_temp_file "OtterTest." ".c" begin fun (temppath, tempout) ->
            close_out tempout;

            (* TODO: add standard search paths to otter.pl *)
            if (Sys.command ("./otter.pl -nostdlib -I./libc/ -include./libc/__otter/all.h -E -o"^temppath^" "^fullpath^" 2>/dev/null")) <> 0 then
                assert_failure "Preprocessor parse error.";

            test_otter_on_file temppath ~main_loop begin fun file results -> 
                (* count jobs that were abandoned *)
                let abandoned = List.fold_left begin fun abandoned result -> match result with
                    | Types.Abandoned (s, loc, _) -> (loc.Cil.file, loc.Cil.line, s)::abandoned
                    | _ -> abandoned
                end [] results in
                if abandoned <> [] then
                    let printer = list_printer (fun ff (f, l, s) -> Format.fprintf ff "@[%s:%d: %s@]" f l s) "@\n" in
                    assert_failure "@[<hv2>Abandoned paths:@\n%a@]" printer abandoned
            end ()

        end

    end


(*
 * OUnit test suite
 *)

let testsuite = "System" >::: [
	test_integration Driver.init_with_libc "OtterCore";
]

