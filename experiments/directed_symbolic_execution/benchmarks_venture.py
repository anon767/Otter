benchmarks = {
    #'coreutils-conrete' : {
    #    'programs' : {
    #        'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540  \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_tac.c"',
    #        'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672 \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_pr.c"',
    #        },
    #    'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=1800'
    #    }
    #'coreutils-conrete-bounded' : {
    #    'programs' : {
    #        'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540  \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.tac \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_tac.c"',
    #        'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672 \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.pr \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_pr.c"',
    #        },
    #    'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=1800'
    #    }
    #'coreutils-semiconrete-bounded' : {
    #    'programs' : {
    #        'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540  \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.tac \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_tac_semi.c"',
    #        'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672 \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.pr \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_pr_semi.c"',
    #        },
    #    'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=1800'
    #    }
    #'coreutils-almost-regular-bounded' : {
    #    'programs' : {
    #        'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540  \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.tac \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_tac_regular.c"',
    #        'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672 \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.pr \
    #                    "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_pr_regular.c"',
    #        },
    #    'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=7200'
    #    }
    'coreutils-regular' : {
        'programs' : {
            'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
                        -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
                        --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540',
            'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
                        -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
                        --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" \
                     "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=14400'
        },
    #'coreutils-regular-bounded' : {
    #    'programs' : {
    #        'tac'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,7,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:540 \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.tac',
    #        'pr'     : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c"  \
    #                    -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
    #                    --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672 \
    #                    --external-bounding-paths=@trunk@/experiments/directed_symbolic_execution/path.pr',
    #        },
    #    'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" \
    #                 "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=14400'
    #    },
    }

strategies = {
    #'SDSERP'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=SDSE-RP',
    #'BoundedInterSDSE'  : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=bounded-backotter-closest-to-targets',
    #'BoundedPathWeighted' : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=bounded-path-weighted',
    # 'InterSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets',
    # #'IntraSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets-intraprocedural',
    # 'OtterKLEE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=KLEE',
    # 'OtterSAGE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=SAGE',
    # 'RandomPath'        : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=random-path',
    'CCBSE(RandomPath)' : '--dobackotter --function-inlining --forward-queue=random-path --backward-queue=random-path --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    #'CCBSE(InterSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets --backward-queue=backotter-closest-to-targets --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    #'CCBSE(IntraSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets-intraprocedural --backward-queue=backotter-closest-to-targets-intraprocedural --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    'Mix(OtterKLEE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=KLEE',
    ##'Mix(OtterSAGE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=SAGE',
    'Mix(RandomPath)'   : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=random-path',
}

options = {
    'std'  : '--convert-non-target-reached-abandoned-to-truncated --doRunRmtmps --init-malloc=undefined --init-local=undefined --max-abandoned=1 --backotter-timing-method=weighted --backotter-no-overlap-path-matching --console-width=300',
}

def make_test(command, program, strategy, option, seed):
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_csv  = '"@dest@/csv/%s/%s/%s/%d/entry"' % (option[0], program[0], strategy[0], seed)

    test_cmd  = 'mkdir -p $(dirname %s)' % test_log + "\n"
    test_cmd += 'mkdir -p $(dirname %s)' % test_csv + "\n"
    test_cmd += '%s %s %s %s --random-seed=%d 2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s' % (command, strategy[1], option[1], program[1], seed, test_log) + "\n"
    test_cmd += 'cat %s | @trunk@/experiments/directed_symbolic_execution/targetreached.py > %s' % (test_log, test_csv)
    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

