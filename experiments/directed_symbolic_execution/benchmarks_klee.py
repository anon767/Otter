import os

benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mkdir.bc"  --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            'mkfifo' : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mkfifo.bc" --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            'mknod'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mknod.bc"  --sym-args 0 1 10 --sym-args 0 3 2 --sym-files 1 8 --sym-stdout ',
            'paste'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/paste.bc"  --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            'ptx'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/ptx.bc"    --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            'seq'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/seq.bc"    --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            #'tac'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/tac.bc"    --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',
            },
        'command' : 'klee --max-time=1800 --libc=uclibc --posix-runtime --allow-external-sym-calls'
        },
    'synthetic' : {
        'programs' : {
            'ProSDSE'  : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_distance_1.o",
            'ProCCBSE' : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_backotter_3.o",
            'ProMix'   : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_mix_1.o",
            },
        'command' : 'klee --max-time=600'
        }
    }

strategies = {
    'KLEE'  : '',
}

options = {
    'std'  : '--simplify-sym-indices --output-module --max-memory=1000 --disable-inlining --optimize --use-forked-stp --use-cex-cache --only-output-states-covering-new --max-sym-array-size=4096 --max-instruction-time=10.  --watchdog --max-memory-inhibit=false --max-static-fork-pct=1 --max-static-solve-pct=1 --max-static-cpfork-pct=1 --switch-type=internal --randomize-fork --use-random-path --use-interleaved-covnew-NURS --use-batching-search --batch-instructions 10000',
}

def make_test(command, program, strategy, option, seed):
    klee_dir  = '"@dest@/logs/%d/%s-%s-%s.kleeout"' % (seed, program[0], strategy[0], option[0])
    klee_sandbox  = '"@dest@/logs/%d/%s-%s-%s.sandbox"' % (seed, program[0], strategy[0], option[0])
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_csv  = '"@dest@/csv/%s/%s/%s/%d/entry"' % (option[0], program[0], strategy[0], seed)

    test_cmd  = 'mkdir -p $(dirname %s)' % test_log + "\n"
    test_cmd += 'mkdir -p $(dirname %s)' % test_csv + "\n"
    test_cmd += 'mkdir -p %s' % klee_sandbox + "\n"
    test_cmd += '%s --run-in=%s --output-dir=%s %s %s %s 2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s' % (command, klee_sandbox, klee_dir, strategy[1], option[1], program[1], test_log) + "\n"
    test_cmd += 'cat %s | @trunk@/experiments/directed_symbolic_execution/kleeasserterr.py > %s' % (test_log, test_csv)
    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

