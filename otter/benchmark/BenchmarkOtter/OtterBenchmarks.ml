open TestUtil.MyOUnit
open TestUtil.OtterPragmaTests
open OtterCore
open OtterQueue
open OtterDriver
open Otter
open BackOtter

module BackOtterPragmaTest = TestUtil.OtterPragmaTests.Make (BackOtter.BackOtterErrors)


(**/**)
let (>>>) = Interceptor.(>>>)
(**/**)


(* directory containing tests for Benchmarks *)
let dir = Filename.concat "benchmark" (Filename.concat "BenchmarkOtter" "Benchmarks")


(* benchmarks as an OUnit test suite *)
let benchmarks =
    "Benchmarks" >: test_dir dir begin fun relpath ->

        (* load the file at fullpath, but label with relpath *)
        let fullpath = Filename.concat dir relpath in

        (* benchmarks have to be set up individually due to differences in the types of drivers *)
        let benchmark driver =
            (* TODO: avoid preprocessing the file repeatedly *)
            test_with_temp_file "OtterBenchmark." ".c" begin fun (temppath, tempout) ->
                close_out tempout;

                (* TODO: add standard search paths to otter.pl *)
                if (Sys.command ("./otter.pl -nostdinc -isystem./libc/ -include./libc/__otter/all.h -E -o"^temppath^" "^fullpath^" 2>/dev/null")) <> 0 then
                    assert_failure "Preprocessor parse error.";

                (* tests need to be evaluated consistently, so use BackOtter's error reasons for all tests as they are richer *)
                let reporter = BackOtterPragmaTest.eval_otter_with_pragma driver (fun () -> new BenchmarkUtil.BenchmarkingReporter.t) temppath () in
                reporter#summarize
            end
        in

        (* use BackOtter's interceptors so the same types of errors can be detected *)
        let interceptor = BackOtterDriver.otter_failure_interceptor >>> BuiltinFunctions.libc_interceptor >>> BuiltinFunctions.interceptor in
        relpath >:::
            List.map begin fun (name, queue) ->
                "Otter:" ^ name >:: benchmark (Driver.run ~interceptor ~queue:(Queue.get queue))
            end Queue.queues
            @
            [
                "BackOtter" >:: benchmark BackOtterDriver.callchain_backward_se;
            ]
    end

