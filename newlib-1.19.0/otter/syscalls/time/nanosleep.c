#include <otter/otter_scheduler.h>
#include <time.h>
#include <errno.h>

/* Pretends that Otter runs one statement per nanosecond. If this makes
	 something take too long to run, define this to a smaller number */
#ifndef OTTER_TICKS_PER_SEC
#define OTTER_TICKS_PER_SEC 1000000000
#endif

int nanosleep(struct timespec const *rqtp, struct timespec *rmtp)
{
	if(rqtp->tv_sec < 0 || rqtp->tv_nsec < 0 || rqtp->tv_nsec >= 1000000000)
	{
		errno = EINVAL;
		return(-1);
	}
	// TODO: we should increment Otter's internal sense of the time, also.
	while (rqtp->tv_sec--) {
		__otter_multi_time_wait(OTTER_TICKS_PER_SEC);
	}
	// This is conceptually what we want, but doesn't overflow ruin it?
	__otter_multi_time_wait((rqtp->tv_nsec * OTTER_TICKS_PER_SEC) / 1000000000);
	return(0);
}
