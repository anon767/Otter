#include "otter_builtins.h"
#include <sys/time.h>

static time_t __otter_syscalls_current_time;

/* create a larger time every time the time is asked for */
static time_t __otter_syscalls_get_current_time()
{
	time_t t;
	__SYMBOLIC(&t);
	__ASSUME(t > __otter_syscalls_current_time);
	__otter_syscalls_current_time = t;
	return(t);
}

int _gettimeofday_r(struct timeval *tv, void *tzp)
{
	__ASSERT(tzp == 0);
	tv->tv_sec = __otter_syscalls_get_current_time();
	tv->tv_usec = 0;
	return(0);
}
