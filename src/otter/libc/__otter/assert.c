#include <stdlib.h>
#include <string.h>
#include "otter.h"

void __otter_libc_failwith(char *msg)
{
	__EVALSTR(msg, strlen(msg));
	abort();
}
