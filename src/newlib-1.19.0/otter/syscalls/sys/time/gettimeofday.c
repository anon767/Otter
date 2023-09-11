#include "otter/otter_builtins.h"
#include "otter/otter_scheduler.h"
#include <sys/time.h>

/* create a larger time every time the time is asked for */
static unsigned long long __otter_syscalls_get_current_time()
{
	static unsigned long long __otter_syscalls_current_time; // This gets statically initialized to 0
#ifdef OTTER_SYMBOLIC_TIME
	unsigned long long t;
	__SYMBOLIC(&t);
	__ASSUME(t > __otter_syscalls_current_time);
	__otter_syscalls_current_time = t;
	return(t);
#else
	return __otter_syscalls_current_time++;
#endif
}

int gettimeofday(struct timeval *tv, void *tzp)
{
	__ASSERT(tzp == 0);
	unsigned long long now = __otter_syscalls_get_current_time();
	tv->tv_sec = now >> LOG2_OTTER_TICKS_PER_SECOND; // This is equivalent to (now / OTTER_TICKS_PER_SECOND)
	tv->tv_usec = now & (OTTER_TICKS_PER_SECOND - 1); // This is equivalent to (now % OTTER_TICKS_PER_SECOND)
	/* The expression above gives us the number of *ticks* remaining beyond
	a whole number of seconds. If we wanted to be really careful about
	modeling time, we might want to convert this number to microseconds by
	doing
	tv->tv_usec = ((now & (OTTER_TICKS_PER_SECOND - 1)) * 1000000) >> LOG2_OTTER_TICKS_PER_SECOND
	but this could fail because of overflow. Besides, we almost certainly
	don't need to be this pedantic about time. */
	return(0);
}
