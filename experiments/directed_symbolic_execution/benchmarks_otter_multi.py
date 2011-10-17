benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2 \
                        --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/mkdir.c.gcov',
            'mkfifo' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2 \
                        --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/mkfifo.c.gcov',
            'mknod'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS=1,10,2,2,2 \
                        --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/mknod.c.gcov',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=zero --init-local=zero '
        },
    'coreutils2' : {
        'programs' : {
            'seq'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/seq_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2 \
                         --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/seq.c.gcov \
                         --noUseLogicalOperators',
            'md5sum' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/md5sum_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
                          --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/md5sum.c.gcov',
            'paste'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/paste_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2  \
                         --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/paste.c.gcov',
            'ptx'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/ptx_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2 \
                          --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/ptx.c.gcov',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=zero --init-local=zero --unsound-array-replace-single-element '
        },
    'coreutils-pr' : {
        'programs' : {
            'pr'   : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c" -DMAX_ARGC=3 -DMAX_ARG_LENGTHS=1,2,1 \
                       --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/pr.c.gcov \
                       -D__OTTER_FIXED_ARGC --unsound-pointer-arithmetic -D__OTTER_MAX_FILE_SIZE=2 -D__OTTER_SETUP_PWD',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=zero --init-local=zero -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO'
        },
    'coreutils-tac' : {
        'programs' : {
            'tac'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,2,2,2 \
                      --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/tac.c.gcov',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=symbolic --init-local=symbolic -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO -D__OTTER_MAX_FILE_SIZE=1 -D__OTTER_FIXED_ARGC'
        },
    }

strategies = {
    'InterSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets',
    'InterSDSE(RoundRobin)'   : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-round-robin-closest-to-targets',
    'InterSDSE(Probabilistic)': '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-probabilistic-closest-to-targets',
    'IntraSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets-intraprocedural',
    'CCBSE(RandomPath)' : '--dobackotter --function-inlining --forward-queue=random-path --backward-queue=random-path --bidirectional-search-ratio=-1 --backward-function-rank=closest-to-entry',
    'CCBSE(InterSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets --backward-queue=backotter-closest-to-targets --bidirectional-search-ratio=-1 --backward-function-rank=closest-to-entry',
    'CCBSE(IntraSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets-intraprocedural --backward-queue=backotter-closest-to-targets-intraprocedural --bidirectional-search-ratio=-1 --backward-function-rank=closest-to-entry',
    'OtterKLEE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=KLEE',
    'Mix(OtterKLEE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --backward-function-rank=closest-to-entry --forward-queue=KLEE',
    'OtterSAGE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=SAGE',
    'Mix(OtterSAGE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --backward-function-rank=closest-to-entry --forward-queue=SAGE',
    'RandomPath'        : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=random-path',
    'Mix(RandomPath)'   : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --backward-function-rank=closest-to-entry --forward-queue=random-path',
}

options = {
    'multi'  : '--fork-finish-at-targets --remove-line-targets-once-found --convert-non-target-reached-abandoned-to-truncated --doRunRmtmps --printErrorsOnly --backotter-timing-method=weighted --backotter-no-overlap-path-matching --function-job-points-to=really-unsound-typed-void --unsound-array-size=8 ',
}
# --unsound-array-size=8 \
# --unsound-array-for-all-types \

def make_test(command, program, strategy, option, seed):
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_csv  = '"@dest@/csv/%s/%s/%s/%d/entry"' % (option[0], program[0], strategy[0], seed)
    test_gcov = '"@dest@/logs/%d/%s-%s-%s.gcov"' % (seed, program[0], strategy[0], option[0])
    test_comb = '"@dest@/logs/%d/%s-%s-%s.comb.c"' % (seed, program[0], strategy[0], option[0])

    test_cmd  = 'mkdir -p $(dirname %s)' % test_log + "\n"
    test_cmd += 'mkdir -p $(dirname %s)' % test_csv + "\n"
    test_cmd += '%s %s %s %s --random-seed=%d --gcov --gcov-path="@trunk@/newlib-1.19.0/otter" --gcov-out=%s --out=%s 2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s' % (command, strategy[1], option[1], program[1], seed, test_gcov, test_comb, test_log) + "\n"
    test_cmd += 'cat %s | @trunk@/experiments/directed_symbolic_execution/targetreached.py > %s' % (test_log, test_csv)
    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

