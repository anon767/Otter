make clean
CILLY_DONT_COMPILE_AFTER_MERGE= make CC="../../cil/bin/cilly --merge -nostdinc -isystem../../otter/libc/ -include../../otter/libc/__otter/all.h --out=vsftpd.cil.c"
