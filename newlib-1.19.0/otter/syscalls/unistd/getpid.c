#include <unistd.h>
#include "otter/multiotter_builtins.h"

pid_t getpid()
{
	return __otter_multi_get_pid();
}
