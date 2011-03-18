#!/bin/sh
CFLAGS="-g -fprofile-arcs -ftest-coverage"

gcc ../../__otter_poi.c -c $CFLAGS
../../../configure --disable-nls CFLAGS="$CFLAGS"
make CC="gcc $PWD/__otter_poi.o"
