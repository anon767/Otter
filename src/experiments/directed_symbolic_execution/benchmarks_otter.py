# Bug reports:
# pr contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=6856089f7bfaca2709b303f01dae001a30930b61
# tac http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=d701f6abb73e36721de5df083df4769786a14528
# Detail: the problem occurs at the *second* call to re_search() at tac.c:267 which happens if tac is given the -r option and two files (the second file leads to the second call to re_search()). The first call initializes the local regs at tac.c:215 and sets bufp->regs_allocated to REGS_REALLOCATE (originally REGS_UNALLOCATED) at regexec.c:489. The second call to re_search() at tac.c:267 is called with a *different* regs (since a local variable), but bufp->regs_allocated is now REGS_REALLOCATE, which leads to regexec.c:540. If regs->num_regs is greater than need_regs, then regs will remain uninitialized, which leads to an invalid pointer dereference at regexec.c:568.
# The underlying cause of the bug is an implicit dependence between two data structures, which cannot be tested for with a simple if-statement, and the bug can actually lead to two different crashes (a realloc with an invalid pointer, or an invalid pointer dereference). We could make the dependence explicit by adding a field, but we're not sure if we should do that as it changes the program semantics a little more invasively. Ideally, we'd have a more specific way of detecting errors than adding "if (cond) __FAILURE()" to programs. the bug requires one field of a local variable (one of the data structures) to be uninitialized and treated as non-zero, but this currently leads to a long unaddressed bug in Otter in the way it detects uninitialized values: it fails if variables containing uninitialized values are converted to STP expressions even if the values are not actually read. We suppose one way around this would be to turn off detection of uninitialized values.
# seq contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=b8108fd2ddf77ae79cd014f4f37798a52be13fd1
# ptx contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=a0851554bd52038ed47e46ee521ce74a5a09f747
# ptx's second bug: http://lists.gnu.org/archive/html/bug-coreutils/2008-07/msg00105.html
# md5sum contains a buffer underflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=7cb24684cc4ef96bb25dfc1c819acfc3b98d9442

benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/quotearg.c:252',
            'mkfifo' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/quotearg.c:252',
            'mknod'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS=1,10,2,2,2 --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/quotearg.c:252',
            'paste'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/paste_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/paste.c:196',
            'ptx'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/ptx_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/ptx.c:312',
            'seq'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/seq_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/seq.c:215 --noUseLogicalOperators',
            'md5sum' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/md5sum_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM  --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/md5sum.c:213',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=1800 --bypassed-functions=main --init-malloc=zero --init-local=zero '
        },
    'coreutils-ptx2' : {
        'programs' : {
            'ptx2' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/ptx_comb.c"  -DMAX_ARGC=3 -DMAX_ARG_LENGTHS=1,2,2 --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/ptx.c:1510 --unsound-pointer-arithmetic -D__OTTER_FIXED_ARGC -D__OTTER_MAX_FILE_SIZE=1',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=1800 --bypassed-functions=main --init-malloc=zero --init-local=zero -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO'
        },
    'coreutils-pr' : {
        'programs' : {
            'pr'   : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/pr_comb.c" -DMAX_ARGC=3 -DMAX_ARG_LENGTHS=1,2,1 --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2674 -D__OTTER_FIXED_ARGC --unsound-pointer-arithmetic -D__OTTER_MAX_FILE_SIZE=2 -D__OTTER_SETUP_PWD',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=zero --init-local=zero -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO'
        },
    'coreutils-tac' : {
        'programs' : {
            'tac'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/tac_comb.c" -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,2,2,2 --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../lib/regexec.c:568',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" --mainfn=__otter_main_driver -lm --timeout=7200 --bypassed-functions=main --init-malloc=symbolic --init-local=symbolic -D__OTTER_SETUP_FILE_SYSTEM -D__OTTER_NO_STDIO -D__OTTER_MAX_FILE_SIZE=1 -D__OTTER_FIXED_ARGC'
        },
    'synthetic' : {
        'programs' : {
            'ProSDSE'  : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_distance_1.c",
            'ProCCBSE' : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_backotter_3.c",
            'ProMix'   : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_mix_1.c",
            },
        'command' : 'CILLY_DONT_COMPILE_AFTER_MERGE= @trunk@/otter/otter.pl --merge -nostdinc -nostdlib -Wp,-undef,-D_POSIX_THREADS,-D__GNUC__,-D_GCC_LIMITS_H_ --timeout=900'
        }
    }

strategies = dict(
        [(s,'--bidirectional-search-ratio=1.1 --forward-queue="%s"'%s) for s in [
            "KLEE",
            "SAGE",
            "random-path",
            "IntraSDSE",
           #"InterSDSE",
            "InterSDSE-probabilistic",
            "InterSDSE-efficient",
           #"RoundRobin(RandomPath,InterSDSE)",
            "RoundRobin(RandomPath,InterSDSE-efficient)",
           #"Batched(InterSDSE)",
            "Batched(InterSDSE-probabilistic)",
            "Batched(InterSDSE-efficient)",
           #"Batched(RoundRobin(RandomPath,InterSDSE))",
            "Batched(RoundRobin(RandomPath,InterSDSE-efficient))",
            "Batched(Phases(KLEE,InterSDSE-efficient))",
            ]] +
        [('CCBSE(%s)'%s,'    --bidirectional-search-ratio=-1  --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue="%s"'%(s,s)) for s in [
            "random-path",
           #"IntraSDSE",
            "InterSDSE",
            ]] +
        [('Mix(%s,0.75)'%s,'  --bidirectional-search-ratio=.75 --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue=random-path '%s) for s in [
            "random-path",
            "KLEE",
            "SAGE",
            ]] +
        #[('Mix(%s)'%s,'      --bidirectional-search-ratio=.5  --function-inlining --backward-function-rank=closest-to-entry --forward-queue="%s" --backward-queue=random-path '%s) for s in [
        #    "random-path",
        #    "KLEE",
        #    "SAGE",
        #    ]] +
        []
        )

options = {
    'std'  : '--dobackotter --convert-non-target-reached-abandoned-to-truncated --doRunRmtmps --max-abandoned=1 --printErrorsOnly --backotter-timing-method=real --function-job-points-to=really-unsound-typed-void ',
}

def make_test(command, program, strategy, option, seed):
    test_log  = '"@dest@/logs/%d/%s-%s-%s.log"' % (seed, program[0], strategy[0], option[0])
    test_csv  = '"@dest@/csv/%s/%s/%s/%d/entry"' % (option[0], program[0], strategy[0], seed)

    test_cmd  = ''
    test_cmd += 'if [ -f %s ]; then\n' % test_log
    test_cmd += '   echo %s " exists"\n' % test_log
    test_cmd += 'else\n'
    test_cmd += '   mkdir -p "$(dirname %s)"\n' % test_log
    test_cmd += '   mkdir -p "$(dirname %s)"\n' % test_csv
    test_cmd += '   %s %s %s %s --random-seed=%d 2>&1 | @trunk@/experiments/directed_symbolic_execution/timelines.py > %s \n' % (command, strategy[1], option[1], program[1], seed, test_log)
    test_cmd += '   cat %s | @trunk@/experiments/directed_symbolic_execution/targetreached.py > %s \n' % (test_log, test_csv)
    test_cmd += 'fi\n'

    test_file = '@dest@/tests/%d/%s-%s-%s.sh' % (seed, program[0], strategy[0], option[0])
    return (test_file, test_cmd)

