/* Include this file to make sure that the appropriate function definitions are available for Otter to substitute in. */

#include "assert.c"
#include "ctype.c"
#include "stdlib.c"
#include "string.c"

/* I/O */
#include "otter_fs.c"
#include "otter_fs_util.c"
#include "stat.c"
#include "fcntl.c"
#include "stdio.c"

#include "unistd.c"
#include "signal.c"
#include "socket.c"
#include "arpa_inet.c"

__otter_libc_init()
{
	__otter_fs_mount();

	open("/dev/tty", O_RDONLY);
	open("/dev/tty", O_WRONLY);
	open("/dev/tty", O_WRONLY);

	return (0);
}
