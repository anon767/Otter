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
            'mkdir'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mkfifo' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mknod'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS=1,10,2,2,2 -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'paste'  : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/paste_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_PASTE_ASSERT',
            'ptx'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/ptx_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_COPY_UNESCAPED_STRING_BOUNDS_CHECKING',
            'seq'    : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/seq_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_LONG_DOUBLE_FORMAT_BOUNDS_CHECKING --noUseLogicalOperators',
            'md5sum' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/md5sum_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
                        --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/md5sum.c:213',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver.c" "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=1800 --bypassed-functions=main'
        },
    'coreutils-customized' : {
        'programs' : {
            'ptx2' : '"@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/build.cil/src/ptx_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS=1,10,2,2   -D__OTTER_SETUP_FILE_SYSTEM \
                      "@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_main_driver_ptx_customized.c" \
                      --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/ptx.c:1489 \
                      --line-targets=@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/ptx.c:1510 \
                      --minusPP-compare-blocks-by-addr',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc""@trunk@/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver -lm --timeout=1800 --bypassed-functions=main'
        },
    'synthetic' : {
        'programs' : {
            'ProSDSE'  : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_distance_1.c",
            'ProCCBSE' : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_backotter_3.c",
            'ProMix'   : "@trunk@/experiments/directed_symbolic_execution/synthetic/pro_mix_1.c",
            },
        'command' : 'CILLY_DONT_COMPILE_AFTER_MERGE= @trunk@/otter/otter.pl --merge -nostdinc -nostdlib -Wp,-undef,-D_POSIX_THREADS,-D__GNUC__,-D_GCC_LIMITS_H_ --timeout=600'
        }
    }

strategies = {
    'InterSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets',
    'IntraSDSE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=backotter-closest-to-targets-intraprocedural',
    'CCBSE(RandomPath)' : '--dobackotter --function-inlining --forward-queue=random-path --backward-queue=random-path --bidirectional-search-ratio=-1 --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry',
    'CCBSE(InterSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets --backward-queue=backotter-closest-to-targets --bidirectional-search-ratio=-1 --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry',
    'CCBSE(IntraSDSE)'  : '--dobackotter --function-inlining --forward-queue=backotter-closest-to-targets-intraprocedural --backward-queue=backotter-closest-to-targets-intraprocedural --bidirectional-search-ratio=-1 --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry',
    'OtterKLEE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=KLEE',
    'Mix(OtterKLEE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=KLEE',
    'OtterSAGE'         : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=SAGE',
    'Mix(OtterSAGE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=SAGE',
    'RandomPath'        : '--dobackotter --bidirectional-search-ratio=1.1 --forward-queue=random-path',
    'Mix(RandomPath)'   : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=really-unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=random-path',
}

options = {
    'std'  : '--convert-non-target-reached-abandoned-to-truncated --doRunRmtmps --init-malloc=zero --init-local=zero --max-abandoned=1 --printErrorsOnly --backotter-timing-method=weighted --backotter-no-overlap-path-matching',
}

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

