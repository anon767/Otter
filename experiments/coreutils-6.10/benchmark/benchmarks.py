# Bug reports:
# pr contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=6856089f7bfaca2709b303f01dae001a30930b61
# tac http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=d701f6abb73e36721de5df083df4769786a14528
# Detail: the problem occurs at the *second* call to re_search() at tac.c:267 which happens if tac is given the -r option and two files (the second file leads to the second call to re_search()). The first call initializes the local regs at tac.c:215 and sets bufp->regs_allocated to REGS_REALLOCATE (originally REGS_UNALLOCATED) at regexec.c:489. The second call to re_search() at tac.c:267 is called with a *different* regs (since a local variable), but bufp->regs_allocated is now REGS_REALLOCATE, which leads to regexec.c:540. If regs->num_regs is greater than need_regs, then regs will remain uninitialized, which leads to an invalid pointer dereference at regexec.c:568.
# The underlying cause of the bug is an implicit dependence between two data structures, which cannot be tested for with a simple if-statement, and the bug can actually lead to two different crashes (a realloc with an invalid pointer, or an invalid pointer dereference). We could make the dependence explicit by adding a field, but we're not sure if we should do that as it changes the program semantics a little more invasively. Ideally, we'd have a more specific way of detecting errors than adding "if (cond) __FAILURE()" to programs. the bug requires one field of a local variable (one of the data structures) to be uninitialized and treated as non-zero, but this currently leads to a long unaddressed bug in Otter in the way it detects uninitialized values: it fails if variables containing uninitialized values are converted to STP expressions even if the values are not actually read. We suppose one way around this would be to turn off detection of uninitialized values.
# seq contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=b8108fd2ddf77ae79cd014f4f37798a52be13fd1
# ptx contains a buffer overflow : http://git.savannah.gnu.org/cgit/coreutils.git/commit/?id=a0851554bd52038ed47e46ee521ce74a5a09f747

benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '"@trunk@/experiments/coreutils-6.10/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mkfifo' : '"@trunk@/experiments/coreutils-6.10/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mknod'  : '"@trunk@/experiments/coreutils-6.10/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS="{1,10,2,2,2}" -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'paste'  : '"@trunk@/experiments/coreutils-6.10/src/paste_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_PASTE_ASSERT',
            'ptx'    : '"@trunk@/experiments/coreutils-6.10/src/ptx_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_COPY_UNESCAPED_STRING_BOUNDS_CHECKING',
            'seq'    : '"@trunk@/experiments/coreutils-6.10/src/seq_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_LONG_DOUBLE_FORMAT_BOUNDS_CHECKING --noUseLogicalOperators',
           #'mkdir-inj'  : '"@trunk@/experiments/coreutils-6.10/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_INJECTED_MAKE_NODE_OP_EQUALS_ASSERT',
           #'mkfifo-inj' : '"@trunk@/experiments/coreutils-6.10/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_INJECTED_MAKE_NODE_OP_EQUALS_ASSERT',
           #'mknod-inj'  : '"@trunk@/experiments/coreutils-6.10/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS="{1,10,2,2,2}" -D__OTTER_INJECTED_MAKE_NODE_OP_EQUALS_ASSERT',
            },
        'command' : '"@trunk@/newlib-1.19.0/otter/otter-with-libc" "@trunk@/experiments/coreutils-6.10/benchmark/__otter_main_driver.c" "@trunk@/experiments/coreutils-6.10/benchmark/__otter_poi.c" --mainfn=__otter_main_driver --timeout=1800'
        },
    'synthetic' : {
        'programs' : {
            'pro_backotter_3' : "@TRUNK@/experiments/coreutils-6.10/benchmark/synthetic/pro_backotter_3.c",
            'pro_distance_1'  : "@TRUNK@/experiments/coreutils-6.10/benchmark/synthetic/pro_distance_1.c",
            },
        'command' : 'CILLY_DONT_COMPILE_AFTER_MERGE= @trunk@/otter/otter.pl --merge -nostdinc -nostdlib -Wp,-undef,-D_POSIX_THREADS,-D__GNUC__,-D_GCC_LIMITS_H_ --timeout=120'
        }
    }

strategies = {
    'InterSDSE'         : '--dootter --queue=closest-to-targets',
    'IntraSDSE'         : '--dootter --queue=closest-to-targets-intraprocedural',
    'CCBSE(RandomPath)' : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    'CCBSE(InterSDSE)'  : '--dobackotter --function-inlining --backward-queue=backotter-closest-to-targets --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    'CCBSE(IntraSDSE)'  : '--dobackotter --function-inlining --backward-queue=backotter-closest-to-targets-intraprocedural --bidirectional-search-ratio=-1 --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry',
    'OtterKLEE'         : '--dootter --queue=KLEE',
    'Mix(OtterKLEE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=KLEE',
    'OtterSAGE'         : '--dootter --queue=SAGE',
    'Mix(OtterSAGE)'    : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=SAGE',
    'RandomPath'        : '--dootter --queue=random-path',
    'Mix(RandomPath)'   : '--dobackotter --function-inlining --backward-queue=random-path --bidirectional-search-ratio=.5  --function-job-points-to=unsound-typed-void --backward-function-rank=closest-to-entry --forward-queue=random-path',
}

options = {
    'std'  : '--doRunRmtmps --initMallocZero --initLocalZero --max-abandoned=1 --convert-non-failure-abandoned-to-truncated --printErrorsOnly --backotter-timing-method=stp-calls',
}

