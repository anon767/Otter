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
#include "sys_mman.c"
#include "sys_uio.c"
#include "syslog.c"
#include "grp.c"
#include "pwd.c"
#include "time.c"
#include "wait.c"
#include "dirent.c"

__otter_libc_init()
{
	__otter_fs_mount();

	stdin = fopen("/dev/tty", "r");
	stdout = fopen("/dev/tty", "w");
	stderr = fopen("/dev/tty", "w");

	return (0);
}
