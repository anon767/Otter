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

(* options *)
let options =
    BackOtter.BackOtterDriver.options @
    BackOtter.BackOtterQueue.options @
    BackOtter.BackOtterReporter.options @
    BackOtter.BackwardRank.options @
    BackOtter.BidirectionalQueue.options @
    OtterBytes.Stp.options @
    OtterCore.Executeargs.options @
    OtterJob.Job.options @
    OtterQueue.Queue.options @
    OtterReporter.BasicReporter.options


(* benchmarks as an OUnit test suite *)
let benchmarks ?(div_num=1) ?(div_base=1) argv_array =
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

        (* Not sure why using the default current variable will disable stdout *)
        let current = ref 0 in
        Arg.parse_argv ~current argv_array (Arg.align options) (fun _ -> ()) "Usage:";

        let otter_drivers =
            (* don't want depth-first, it's really terrible *)
            let otter_queues = List.filter (fun (_, queue) -> queue <> `DepthFirst) OtterQueue.Queue.queues in
            let otter_driver name queue =
                "Otter:" ^ name >:: benchmark (Driver.run ~interceptor ~queue:(OtterQueue.Queue.get queue))
            in
            List.map begin fun (name, queue) -> otter_driver name queue end otter_queues
        in

        let backotter_drivers =
            let pure_backotter_driver name brank bqueue =
                (Printf.sprintf "BackOtter:%s" name) >:: benchmark (
                    let targets_ref = ref BackOtterTargets.empty in
                    BackOtterDriver.callchain_backward_se ~targets_ref
                                                          ~b_queue:(BackOtter.BackOtterQueue.get_function_backward_queue targets_ref brank bqueue)
                                                          ~ratio:(-. 0.1) (* Pure backward *)
                )
            in
            let backotter_driver name fqueue brank bqueue ratio =
                (Printf.sprintf "BackOtter:%s" name) >:: benchmark (
                    let targets_ref = ref BackOtterTargets.empty in
                    BackOtterDriver.callchain_backward_se ~targets_ref
                                                          ~f_queue:(BackOtter.BackOtterQueue.get targets_ref fqueue)
                                                          ~b_queue:(BackOtter.BackOtterQueue.get_function_backward_queue targets_ref brank bqueue)
                                                          ~ratio
                )
            in
            (* don't want depth-first, it's really terrible *)
            let backotter_bqueues = List.filter (fun (_, queue) -> queue <> `DepthFirst) BackOtter.BackOtterQueue.queues in
            let backotter_fqueues = List.filter (fun (_, queue) -> queue <> `DepthFirst) BackOtter.BackOtterQueue.queues in

            let rec compose fn lst = function
                | head :: tail -> (List.map (fun ele -> fn ele head) lst) @ (compose fn lst tail)
                | [] -> []
            in
            let backotter_brqueues = compose (fun (rank_name, rank_fn) (bqueue_name, bqueue) ->
                (Printf.sprintf "%s,%s" rank_name bqueue_name, rank_fn, bqueue))
                BackOtter.BackwardRank.queues
                backotter_bqueues
            in
            let backotter_combined_queues = compose (fun (fqueue_name, fqueue) (bqueue_name, rank_fn, bqueue) ->
                (Printf.sprintf "forward(%s),backward(%s)" fqueue_name bqueue_name), fqueue, rank_fn, bqueue)
                backotter_fqueues
                backotter_brqueues
            in
            let backotter_combined_queues = compose (fun (name, fqueue, rank_fn, bqueue) ratio ->
                (Printf.sprintf "%s,ratio(%.2f)" name ratio), fqueue, rank_fn, bqueue, ratio)
                backotter_combined_queues
                [0.75; 0.5; 0.25]
            in
            (List.map begin fun (name, fqueue, brank, bqueue, ratio) -> backotter_driver name fqueue brank bqueue ratio; end backotter_combined_queues) @
            (List.map begin fun (name, brank, bqueue) -> pure_backotter_driver (Printf.sprintf "forward(N/A),backward(%s),ratio(0.0)" name) brank bqueue end backotter_brqueues)
        in

        let drivers = otter_drivers @ backotter_drivers in

        let division div_base div_num lst =
            let div_num = div_num - 1 in (* div_num is now in [0 .. (div_base-1)] *)
            let rec division current lst app =
                match lst with
                | [] -> app
                | head :: tail ->
                    let app = if current = div_num then head :: app else app in
                    division ((current + 1) mod div_base) tail app
            in
            let lst = division 0 lst [] in
            List.rev_append lst []
        in

        relpath >::: (division div_base div_num drivers)

    end

