#include <time.h>
#include <utime.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>

time_t __otter_libc_current_time = 0;
/* create a larger time evertime the time is asked for */
time_t __otter_libc_next_time()
{
	time_t t = 0;
	__SYMBOLIC(&t);
	__ASSUME(t > __otter_libc_current_time);
	__otter_libc_current_time = t;
	return(t);
}

time_t time(time_t *tloc)
{
	time_t t = __otter_libc_next_time();

	if(tloc != NULL)
	{
		*tloc = t;
	}
	
	return(t);
}

void tzset()
{
	/* do nothing */
}

struct tm* localtime_r(const time_t *clock, struct tm *result)
{
	time_t t = *clock;
	result->tm_sec = t % 60;
	result->tm_min = (t / 60) % 60;
	result->tm_hour = (t / 3600) % 24;
	result->tm_mday = (t / 86400) % 28; /* months with 28 days exactly */
	result->tm_mon = (t / 2419200) % 12;
	result->tm_year = (t / 29030400);
	result->tm_wday = (t / 86400) % 7;
	result->tm_yday = (t / 86400) % 48;
	result->tm_isdst = 0;
	result->tm_gmtoff = 0;
	result->tm_zone = "ZZZ";
	result->tm_utime = *clock;
	return result;
}

struct tm* localtime(const time_t *timer)
{
	struct tm* r = malloc(sizeof(struct tm));
	return localtime_r(&timer, r);
}

struct tm *gmtime_r(const time_t *clock, struct tm *result)
{
	return localtime_r(clock, result);
}

struct tm *gmtime(time_t const *timer)
{
	struct tm* r = malloc(sizeof(struct tm));
	return gmtime_r(timer, r);
}

size_t strftime(char *s, size_t maxsize, const char *format, const struct tm *timeptr)
{
	if (maxsize >= 6)
	{
		strcpy(s, "+0123");
		return 5;
	}
	return 0;
}

int gettimeofday(struct timeval *tv, void *tzp)
{
	tv->tv_sec = __otter_libc_next_time();
	tv->tv_usec = 0;
	return(0);
}

int nanosleep(struct timespec  const  *rqtp, struct timespec *rmtp)
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

time_t mktime(struct tm *timeptr)
{
	return(timeptr->tm_utime);
}

int utime(const char *path, const struct utimbuf *times)
{
	return(0);
}

int utimes(const char *path, const struct timeval times[2])
{
	return(0);
}
