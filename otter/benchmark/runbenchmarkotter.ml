open TestUtil.MyOUnit

(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open DataStructures
    open OcamlUtilities
    open CilUtilities
    open OtterBytes
    open OtterCFG
    open OtterCore
    open OtterExtensions
    open OtterJob
    open OtterQueue
    open OtterReporter
    open OtterDriver
    open Otter
    open MultiOtter
    open BackOtter
    open BenchmarkUtil
end

let env_random_seed = "OTTER_RANDOM_SEED"
let env_division = "OTTER_DIVISION"

let _ =
    let random_seed =
        try int_of_string (Sys.getenv env_random_seed)
        with Not_found -> failwith "Please provide a random seed"
    in
    Format.printf "Random seed: %d@\n" random_seed;
    let div_num, div_base =
        try
            Scanf.sscanf (try (Sys.getenv env_division) with Not_found -> "1/1") "%d/%d" (fun n b -> n, b)
        with Scanf.Scan_failure _ | Failure _ | End_of_file ->
            failwith "Division is of the form (div_num)/(div_base), e.g., \"2/3\""
    in
    if div_base <= 0 || div_num <= 0 || div_num > div_base then
        failwith "Bad div_num/div_base";
    Format.printf "Division: %d out of %d@\n" div_num div_base;
    run_test_tt_main (BenchmarkOtter.OtterBenchmarks.benchmarks ~div_num ~div_base random_seed)

