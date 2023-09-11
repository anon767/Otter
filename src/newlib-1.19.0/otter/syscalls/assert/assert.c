#include <stdlib.h>
#include <string.h>
#include <otter/otter_builtins.h>

void __libc_failwith(char *msg)
{
	__EVALSTR(msg, strlen(msg));
	abort();
}
