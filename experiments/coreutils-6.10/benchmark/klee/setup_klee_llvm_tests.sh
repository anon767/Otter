#!/bin/sh

if [ $# -ne 3 ]; then
    echo "Usage: ./setup_klee_llvm_tests.sh <klee_llvm_src_dir> <klee_llvm_test_output_dir>"
    exit 1
fi

src_dir="$1"
output_base="$2"
ktest_monitor="$3"

for f in "$src_dir"/*.bc
do
    program="$(basename $f .bc)"
    output_dir="$output_base/$program"
    test_script="$output_dir/run.sh"
    sandbox_dir="$output_dir/klee-sandbox"
    klee_outdir="$output_dir/klee-out"
    stdout="$output_dir/stdout"
    ktest_monitor_log="$output_dir/ktest-monitor.log"

    mkdir -p "$output_dir"
    mkdir -p "$sandbox_dir"

    echo "\
\"$ktest_monitor\" \"$klee_outdir\" > $ktest_monitor_log & 
ktest_monitor_pid=\$! 
/usr/bin/time -v klee \\
    --simplify-sym-indices \\
    --write-cvcs \\
    --write-cov \\
    --output-module \\
    --max-memory=1000 \\
    --disable-inlining \\
    --optimize \\
    --use-forked-stp \\
    --use-cex-cache \\
    --libc=uclibc \\
    --posix-runtime \\
    --allow-external-sym-calls \\
    --only-output-states-covering-new \\
    --run-in=\"$sandbox_dir\" \\
    --output-dir=\"$klee_outdir\" \\
    --max-sym-array-size=4096 \\
    --max-instruction-time=10. \\
    --max-time=3600. \\
    --watchdog \\
    --max-memory-inhibit=false \\
    --max-static-fork-pct=1 \\
    --max-static-solve-pct=1 \\
    --max-static-cpfork-pct=1 \\
    --switch-type=internal \\
    --randomize-fork \\
    --use-random-path \\
    --use-interleaved-covnew-NURS \\
    --use-batching-search \\
    --batch-instructions 10000 \\
    --init-env \"$f\" \\
    --sym-args 0 1 10 \\
    --sym-args 0 2 2 \\
    --sym-files 1 8 \\
    --sym-stdout \\
 > \"$stdout\" 2>&1 
 kill -9 \$ktest_monitor_pid " > "$test_script"

done

at_sh="$output_base/at.sh"
echo "find \"$output_base\" -name run.sh | xargs -P 15 -n 1 sh" >> $at_sh

# Options not supported by new KLEE
# --exclude-cov-file=./../lib/functions.txt 
# --exclude-libc-cov \\
# --with-libc \\
# --with-file-model=release \\
# --environ=test.env \\
