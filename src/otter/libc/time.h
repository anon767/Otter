#ifndef _TIME_H
#define _TIME_H

#include <stddef.h>

#define CLK_TCK 100
#define CLOCKS_PER_TICK 100

#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>

struct tm
{
	int tm_sec;
	int tm_min;
	int tm_hour;
	int tm_mday;
	int tm_mon;
	int tm_year;
	int tm_wday;
	int tm_yday;
	int tm_isdst;
	int tm_gmtoff;
	char* tm_zone;
	time_t tm_utime;
};

struct timespec
{
	time_t tv_sec;
	long tv_nsec;
};

struct itimerspec
{
	struct timespec it_interval;
	struct timespec it_value;
};

char      *asctime(const struct tm *);
char      *asctime_r(const struct tm *, char *);
clock_t    clock(void);
int        clock_getres(clockid_t, struct timespec *);
int        clock_gettime(clockid_t, struct timespec *);
int        clock_settime(clockid_t, const struct timespec *);
char      *ctime(const time_t *);
char      *ctime_r(const time_t *, char *);
double     difftime(time_t, time_t);
struct tm *getdate(const char *);
struct tm *gmtime(const time_t *);
struct tm *gmtime_r(const time_t *, struct tm *);
struct tm *localtime(const time_t *);
struct tm *localtime_r(const time_t *, struct tm *);
time_t     mktime(struct tm *);
int        nanosleep(const struct timespec *, struct timespec *);
size_t     strftime(char *, size_t, const char *, const struct tm *);
char      *strptime(const char *, const char *, struct tm *);
time_t     time(time_t *);
void       tzset(void);

int        daylight = 0;
long int   timezone = 0;
char      *tzname[] = {"std", "dst"};

#endif
