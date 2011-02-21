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
    BackOtter.BackOtterMain.options @
    Otter.Executemain.options 


(* benchmarks as an OUnit test suite *)
let benchmarks ?(div_num=1) ?(div_base=1) argv_array =
    (* Parse argv_array to initialize Otter *)
    Arg.parse_argv ~current:(ref 0) argv_array (Arg.align options) (fun _ -> ()) "Usage:";

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

        let backotter_drivers =
            let pure_backotter_driver name brank bqueue =
                (Printf.sprintf "BackOtter:%s" name) >:: benchmark (
                    BackOtterMain.callchain_backward_se 
                                                          ~b_queue:(BackOtter.BackOtterQueue.get_function_backward_queue brank bqueue)
                                                          ~ratio:(-. 0.1) (* Pure backward *)
                )
            in
            let backotter_driver name fqueue brank bqueue ratio =
                (Printf.sprintf "BackOtter:%s" name) >:: benchmark (
                    BackOtterMain.callchain_backward_se 
                                                          ~f_queue:(BackOtter.BackOtterQueue.get fqueue)
                                                          ~b_queue:(BackOtter.BackOtterQueue.get_function_backward_queue brank bqueue)
                                                          ~ratio
                )
            in
            (* don't want depth-first, it's really terrible *)
            let wanted_queues = [
                `BreadthFirst;
                `RandomPath;
                `ClosestToTargets;
                `KLEE;
                `SAGE;
            ] in
            let wanted_ranks = [
                `ClosestToEntry;
                `ClosestToFailure;
                `Partial `ClosestToEntry;
            ] in
            let backotter_bqueues = List.filter (fun (_, queue) -> List.mem queue wanted_queues) BackOtter.BackOtterQueue.queues in
            let backotter_fqueues = List.filter (fun (_, queue) -> List.mem queue wanted_queues) BackOtter.BackOtterQueue.queues in
            (* Disable RandomFunction for now *)
            let backotter_branks = List.filter (fun (_, queue) -> List.mem queue wanted_ranks) BackOtter.FunctionRanker.queues in

            let rec compose fn lst = function
                | head :: tail -> (List.map (fun ele -> fn ele head) lst) @ (compose fn lst tail)
                | [] -> []
            in
            let backotter_brqueues = compose (fun (rank_name, rank_fn) (bqueue_name, bqueue) ->
                (Printf.sprintf "%s,%s" rank_name bqueue_name, rank_fn, bqueue))
                backotter_branks
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
                (* Disable 0.25 for now *)
                [0.75; 0.5; (* 0.25 *)]
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

