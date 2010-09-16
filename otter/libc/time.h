#ifndef _TIME_H
#define _TIME_H

#include<sys/types.h>

#define NULL 0
#define CLK_TCK 100
#define CLOCKS_PER_TICK 100

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

#endif
