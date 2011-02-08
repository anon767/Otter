#include <time.h>
#include <errno.h>

//Pretends that Otter runs one statment per nanosecond.

int nanosleep(struct timespec const *rqtp, struct timespec *rmtp)
{
	if(rqtp->tv_nsec < 0 || rqtp->tv_nsec > 100000)
	{
		errno = EINVAL;
		return(-1);
	}
	
	int total = rqtp->tv_nsec + rqtp->tv_sec * 1000;
	
	__otter_multi_time_wait(total);
	return(0);
}
