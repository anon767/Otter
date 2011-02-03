#!/bin/sh

trunk=$(pwd)/../..
newlib=$trunk/newlib-1.19.0/otter

gcc_with_libc=$newlib/gcc-with-libc
cilly_with_libc=$newlib/cilly-with-libc
cilly_ar="$trunk/cil/bin/cilly --merge --mode=AR"

./configure CC="$gcc_with_libc" 

# THIS IS A HACK (FIXME)
# Fix an error of wrong macro value due to a crashing conftest in ./configure
sed -i '' 's/#define RMDIR_ERRNO_NOT_EMPTY configure error in rmdir-errno.m4/#define RMDIR_ERRNO_NOT_EMPTY 39/' lib/config.h

make CC="$cilly_with_libc" AR="$cilly_ar" RANLIB="echo cilly_ranlib: "

$trunk/otter/otter.pl --merge --dofindFns -Llib -lcoreutils  > libcoreutils_functions
