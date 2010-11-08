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
                let reporter, exn_opt = BackOtterPragmaTest.eval_otter_with_pragma driver (new BenchmarkUtil.BackOtterBenchmarkingReporter.t) fullpath () in
                reporter#summarize;
                match exn_opt with
                    | Some exn -> raise exn
                    | None -> ()
            end
        in

        let interceptor = BuiltinFunctions.libc_interceptor >>> BuiltinFunctions.interceptor in

        let otter_drivers =
            (* don't want depth-first, it's really terrible *)
            let otter_queues = List.filter (fun (_, queue) -> queue <> `DepthFirst) OtterQueue.Queue.queues in
            let otter_driver name queue =
                "Otter:" ^ name >:: benchmark (Driver.run ~interceptor ~queue:(OtterQueue.Queue.get queue))
            in
            List.map begin fun (name, queue) -> otter_driver name queue end otter_queues
        in

        let backotter_drivers_list =
            (* don't want depth-first, it's really terrible *)
            let backotter_driver name fqueue brank bqueue ratio =
                (Printf.sprintf "BackOtter:%s(%.2f)" name ratio) >:: benchmark (
                    let targets_ref = ref BackOtterTargets.empty in
                    BackOtterDriver.callchain_backward_se ~targets_ref
                                                          ~f_queue:(BackOtter.Queue.get targets_ref fqueue)
                                                          ~b_queue:(BackOtter.Queue.get_function_backward_queue targets_ref brank bqueue)
                                                          ~ratio
                )
            in
            let backotter_queues = List.filter (fun (_, queue) -> queue <> `DepthFirst) BackOtter.Queue.queues in
            let rec compose fn lst = function
                | head :: tail -> (List.map (fun ele -> fn ele head) lst) @ (compose fn lst tail)
                | [] -> []
            in
            let backotter_bqueues = compose (fun (rank_name, rank_fn) (bqueue_name, bqueue) ->
                (Printf.sprintf "%s,%s" rank_name bqueue_name, rank_fn, bqueue))
                BackOtter.BackwardRank.queues
                backotter_queues
            in
            let backotter_combined_queues = compose (fun (fqueue_name, fqueue) (bqueue_name, rank_fn, bqueue) ->
                (Printf.sprintf "forward(%s),backward(%s)" fqueue_name bqueue_name), fqueue, rank_fn, bqueue)
                backotter_queues
                backotter_bqueues
            in
            List.map begin fun (name, fqueue, brank, bqueue) -> [
                backotter_driver name fqueue brank bqueue (-. 0.1);  (* pure-backward *)
                backotter_driver name fqueue brank bqueue 0.5;
                backotter_driver name fqueue brank bqueue 0.75;
                backotter_driver name fqueue brank bqueue 1.0;       (* pure-forward, plus some side-effect *)
            ] end backotter_combined_queues
        in
        let backotter_drivers = List.concat backotter_drivers_list in

        relpath >::: otter_drivers @ backotter_drivers

    end

