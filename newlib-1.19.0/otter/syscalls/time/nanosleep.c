#include <otter/otter_scheduler.h>
#include <time.h>
#include <errno.h>

int nanosleep(struct timespec const *rqtp, struct timespec *rmtp)
{
	if(rqtp->tv_sec < 0 || rqtp->tv_nsec < 0 || rqtp->tv_nsec >= 1000000000)
	{
		errno = EINVAL;
		return(-1);
	}
	// TODO: we should increment Otter's internal sense of the time, also.
	while (rqtp->tv_sec--) {
		__otter_multi_time_wait(OTTER_TICKS_PER_SECOND);
	}
	__otter_multi_time_wait(rqtp->tv_nsec);
	/* If we wanted to be really careful about modeling time, we might want to make the above
	(rqtp->tv_nsec << LOG2_OTTER_TICKS_PER_SECOND) / 1000000000
	but this would likely overflow. Besides, we almost certainly don't need
	to be this pedantic about time. */
	return(0);
}
