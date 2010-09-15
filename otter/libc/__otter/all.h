/* Include this file to make sure that the appropriate function definitions are available for Otter to substitute in. */

#include "assert.c"
#include "ctype.c"
#include "stdlib.c"
#include "string.c"

/* I/O */
#include "otter_fs.c"
#include "stat.c"
#include "fcntl.c"
#include "stdio.c"

#include "unistd.c"
#include "signal.c"

__otter_libc_init()
{
	__otter_fs_mount();
	return (0);
}
