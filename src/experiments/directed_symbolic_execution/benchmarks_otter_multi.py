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
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --bypassed-functions=main --init-malloc=zero --init-local=zero '
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
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --bypassed-functions=main --init-malloc=zero --init-local=zero --unsound-array-replace-single-element '
        },
    'coreutils-pr' : {
        'programs' : {
            'pr'   : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c" -DMAX_ARGC=3 -DMAX_ARG_LENGTHS=1,2,1 \
                       --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/pr.c.gcov \
                       -D__OTTER_FIXED_ARGC --unsound-pointer-arithmetic -D__OTTER_MAX_FILE_SIZE=2 -D__OTTER_SETUP_PWD',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --bypassed-functions=main --init-malloc=zero --init-local=zero -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO'
        },
    'coreutils-tac' : {
        'programs' : {
            'tac'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,2,2,2 \
                      --line-targets-from-gcov=@trunk@/experiments/directed_symbolic_execution/gcov_outputs/tac.c.gcov',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --bypassed-functions=main --init-malloc=symbolic --init-local=symbolic -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO -D__OTTER_MAX_FILE_SIZE=1 -D__OTTER_FIXED_ARGC'
        },
    }

strategies = dict(
        [(s,'--bidirectional-search-ratio=1.1 --forward-queue="%s"'%s) for s in [
            "KLEE",
            "SAGE",
            "random-path",
            #"IntraSDSE",
            #"InterSDSE",
            #"InterSDSE-roundrobin",
            #"InterSDSE-probabilistic",
            #"InterSDSE-efficient",
            #"InterSDSE-roundrobin-efficient",
            #"RoundRobin(RandomPath,InterSDSE)",
            "RoundRobin(RandomPath,InterSDSE-efficient)",
            #"Batched(InterSDSE)",
            "Batched(InterSDSE-roundrobin)",
            "Batched(InterSDSE-probabilistic)",
            "Batched(InterSDSE-efficient)",
            #"Batched(InterSDSE-roundrobin-efficient)",
            #"Batched(RoundRobin(RandomPath,InterSDSE))",
            "Batched(RoundRobin(RandomPath,InterSDSE-efficient))",
            "Batched(RoundRobin(RandomPath,InterSDSE-roundrobin))",
            #"Batched(RoundRobin(RandomPath,InterSDSE-roundrobin-efficient))",
            "Batched(Phases(KLEE,InterSDSE-efficient))",
            #"Batched(Phases(KLEE,InterSDSE-roundrobin))",
            ]] +
        [('CCBSE(%s)'%s,'--bidirectional-search-ratio=-1 --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue="%s"'%(s,s)) for s in [
            "random-path",
            #"IntraSDSE",
            #"InterSDSE",
            ]] +
        #[('Mix(%s)'%s,'--bidirectional-search-ratio=.5 --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue=random-path --backotter-timing-method=stp-calls '%s) for s in [
        #    "random-path",
        #    "KLEE",
        #    "SAGE",
        #    ]] +
        [('Mix(%s,0.75)'%s,'--bidirectional-search-ratio=.75 --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue=random-path --backotter-timing-method=real '%s) for s in [
            "random-path",
            "KLEE",
            "SAGE",
            ]] +
        []
        )

options = {
    'multi'  : '--dobackotter --fork-finish-at-targets --remove-line-targets-once-found --convert-non-target-reached-abandoned-to-truncated --doRunRmtmps --printErrorsOnly --function-job-points-to=really-unsound-typed-void --unsound-array-size=8 --timeout=7200 ',
}
# --unsound-array-size=8 \
# --unsound-array-for-all-types \

def make_test(command, program, strategy, option, seed):
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_gcov = '"@dest@/logs/%d/%s-%s-%s.gcov"' % (seed, program[0], strategy[0], option[0])
    test_comb = '"@dest@/logs/%d/%s-%s-%s.comb.c"' % (seed, program[0], strategy[0], option[0])

    test_cmd  = ''

    test_cmd += 'if [ -f %s ]; then\n' % test_log
    test_cmd += '   echo %s " exists"\n' % test_log
    test_cmd += 'else\n'
    test_cmd += '   mkdir -p "$(dirname %s)"\n' % test_log
    test_cmd += '   %s %s %s %s --random-seed=%d  2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s \n' % (command, strategy[1], option[1], program[1], seed, test_log)
    test_cmd += 'fi\n'

    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

