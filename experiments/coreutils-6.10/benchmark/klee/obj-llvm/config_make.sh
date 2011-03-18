#!/bin/sh
if [ $# -lt 1 ]; then
    echo "Wrong usage"
    exit 1
fi

klee_gcc="$1"
"$klee_gcc" ../../__otter_poi.c -c
../../../configure --disable-nls CFLAGS="-g"
make CC="$klee_gcc $PWD/__otter_poi.o"
../../setup_klee_llvm_tests.sh $PWD/src $PWD/benchmark $PWD/../../ktest-monitor.sh
