#!/usr/bin/env python
import os, os.path, sys

def call(cmd, argv=[]):
    return os.system(cmd + " " + " ".join(argv))

newlib_otter_dir = os.path.dirname(os.path.abspath(sys.argv[0]))
gcc_with_libc = os.path.join(newlib_otter_dir, "gcc-with-libc")
otter_with_libc = os.path.join(newlib_otter_dir, "otter-with-libc")

if not (os.path.exists(gcc_with_libc) and os.path.exists(otter_with_libc)):
    print "Missing %s or %s" % (gcc_with_libc, otter_with_libc)
    sys.exit(1)

ld_with_libc = otter_with_libc + " --doLinkCheck"

argv = sys.argv[1:]
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

