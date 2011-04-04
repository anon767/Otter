benchmarks = {
    'coreutils' : {
        'programs' : {
            'mkdir'  : '"@trunk@/experiments/coreutils-6.10/src/mkdir_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mkfifo' : '"@trunk@/experiments/coreutils-6.10/src/mkfifo_comb.c"  -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'mknod'  : '"@trunk@/experiments/coreutils-6.10/src/mknod_comb.c"   -DMAX_ARGC=5 -DMAX_ARG_LENGTHS="{1,10,2,2,2}" -D__OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT',
            'paste'  : '"@trunk@/experiments/coreutils-6.10/src/paste_comb.c"   -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_PASTE_ASSERT',
            'ptx'    : '"@trunk@/experiments/coreutils-6.10/src/ptx_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_COPY_UNESCAPED_STRING_BOUNDS_CHECKING',
            'seq'    : '"@trunk@/experiments/coreutils-6.10/src/seq_comb.c"     -DMAX_ARGC=4 -DMAX_ARG_LENGTHS="{1,10,2,2}"   -D__OTTER_LONG_DOUBLE_FORMAT_BOUNDS_CHECKING --noUseLogicalOperators',
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

