import os

benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mkdir.bc"  --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # lib/quotearg.c:252
            'mkfifo' : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mkfifo.bc" --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # lib/quotearg.c:252
            'mknod'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/mknod.bc"  --sym-args 0 1 10 --sym-args 0 3 2 --sym-files 1 8 --sym-stdout ',       # lib/quotearg.c:252
            'paste'  : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/paste.bc"  --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # (no match) src/paste.c:196
            'ptx'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/ptx.bc"    --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # src/ptx.c:312
            'seq'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/seq.bc"    --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # src/seq.c:215
            'md5sum' : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/md5sum.bc" --sym-args 0 1 10 --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # src/md5sum.c:216
            'ptx2'   : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/ptx.bc"                      --sym-args 0 2 2 --sym-files 1 8 --sym-stdout ',       # (no output) src/ptx.c:1510
            },                                                                                                                                                                                                       #
        'command' : 'klee --max-time=1800 --libc=uclibc --posix-runtime --allow-external-sym-calls'                                                                                                                  #
        },                                                                                                                                                                                                           #
    'coreutils2' : {                                                                                                                                                                                                 #
        'programs' : {                                                                                                                                                                                               #
            'pr'     : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/pr.bc"     --sym-args 0 1 2  --sym-args 0 1 1 --sym-files 1 8 --sym-stdout ',       # (no output) src/pr.c:2674
            'tac'    : '--init-env "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/klee/build.llvm/src/tac.bc"    --sym-args 0 3 2                   --sym-files 1 8 --sym-stdout ',       # (no output) lib/regexec.c:568
            },
        'command' : 'klee --max-time=7200 --libc=uclibc --posix-runtime --allow-external-sym-calls'
        },
    'synthetic' : {
        'programs' : {
            'ProSDSE'  : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_distance_1.o",
            'ProCCBSE' : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_backotter_3.o",
            'ProMix'   : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_mix_1.o",
            },
        'command' : 'klee --max-time=900'
        }
    }

strategies = {
    'RealKLEE'  : '',
}

options = {
    'std'  : '--simplify-sym-indices --output-module --max-memory=1000 --disable-inlining --optimize --use-forked-stp --use-cex-cache --only-output-states-covering-new --max-sym-array-size=4096 --max-instruction-time=10.  --watchdog --max-memory-inhibit=false --max-static-fork-pct=1 --max-static-solve-pct=1 --max-static-cpfork-pct=1 --switch-type=internal --randomize-fork --use-random-path --use-interleaved-covnew-NURS --use-batching-search --batch-instructions 10000',
}

def make_test(command, program, strategy, option, seed):
    klee_dir  = '"@dest@/logs/%d/%s-%s-%s.kleeout"' % (seed, program[0], strategy[0], option[0])
    klee_sandbox  = '"@dest@/logs/%d/%s-%s-%s.sandbox"' % (seed, program[0], strategy[0], option[0])
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_csv  = '"@dest@/csv/%s/%s/%s/%d/entry"' % (option[0], program[0], strategy[0], seed)

    test_cmd  = ''
    test_cmd += 'if [ -f %s ]; then\n' % test_log
    test_cmd += '   echo %s " exists"\n' % test_log
    test_cmd += 'else\n'
    test_cmd += '   mkdir -p "$(dirname %s)"\n' % test_log
    test_cmd += '   mkdir -p "$(dirname %s)"\n' % test_csv
    test_cmd += '   mkdir -p %s\n' % klee_sandbox
    test_cmd += '   %s --run-in=%s --output-dir=%s %s %s %s 2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s \n' % (command, klee_sandbox, klee_dir, strategy[1], option[1], program[1], test_log)
    #test_cmd += '   cat %s | @trunk@/experiments/directed_symbolic_execution/kleeasserterr.py > %s \n' % (test_log, test_csv)
    test_cmd += 'fi\n'

    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

