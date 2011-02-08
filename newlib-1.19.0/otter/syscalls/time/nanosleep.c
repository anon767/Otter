#include <time.h>
#include <errno.h>

//Pretends that Otter runs one statement per nanosecond.

int nanosleep(struct timespec const *rqtp, struct timespec *rmtp)
{
	if(rqtp->tv_sec != 0 || rqtp->tv_nsec < 0 || rqtp->tv_nsec > 1000000000)
	{
		errno = EINVAL;
		return(-1);
	}
	// TODO: we should increment Otter's internal sense of the time, also.
	__otter_multi_time_wait(rqtp->tv_nsec);
	return(0);
}
