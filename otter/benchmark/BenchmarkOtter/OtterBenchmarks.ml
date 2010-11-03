open TestUtil.MyOUnit
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
            TestUtil.OtterUtil.test_with_preprocessed_file fullpath begin fun fullpath () ->
                (* tests need to be evaluated consistently, so use BackOtter's error reasons for all tests as they are richer *)
                let reporter = BackOtterPragmaTest.eval_otter_with_pragma driver (new BenchmarkUtil.BenchmarkingReporter.t) fullpath () in
                reporter#summarize
            end
        in

        let interceptor = BuiltinFunctions.libc_interceptor >>> BuiltinFunctions.interceptor in
        relpath >:::
            List.map begin fun (name, queue) ->
                "Otter:" ^ name >:: benchmark (Driver.run ~interceptor ~queue:(Queue.get queue))
            end Queue.queues
            @
            [
                "BackOtter" >:: benchmark BackOtterDriver.callchain_backward_se;
            ]
    end

