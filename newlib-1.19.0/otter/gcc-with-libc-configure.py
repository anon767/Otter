#!/usr/bin/env python
# Usage: ./gcc-with-libc-configure.py gcc-with-libc otter-with-libc [args...]
import os, sys

def call(cmd, argv=[]):
    return os.system(cmd + " " + " ".join(argv))

gcc_with_libc = sys.argv[1]
otter_with_libc = sys.argv[2]
ld_with_libc = otter_with_libc + " --doLinkCheck"

argv = sys.argv[3:]
conftest = "conftest.c"

ret = call(gcc_with_libc, argv)

# Run ld_with_libc when gcc command causes linking
if ret == 0 and "-E" not in argv and "-c" not in argv:
    if conftest in argv:
        # This is a hack: configure tests a function f's existence by
        # declaring it via "char f ();" and calling it in main().
        # Cil complains if the declared f has different return type than
        # the one in newlib. The command below takes out the declaration.
        call("sed -i='' 's/char.*();//'", [conftest])
    # Remove -c from the arguments, since otter_with_libc has --merge,
    # and --merge plus -c become -E in cilly.
    ret = call(ld_with_libc, filter(lambda x: x!="-c",argv))

# if ret is too big, an undefined value will be returned.
ret = 1 if ret else 0

sys.exit(ret)

