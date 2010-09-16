#ifndef _SYS_TIME_H
#define _SYS_TIME_H

#include<sys/types.h>

struct timeval
{
	time_t tv_sec;
	suseconds_t tv_usec;
};

struct itimerval
{
	struct timeval it_interval;
	struct timeval it_value;
};

typedef struct
{
	long fd_bits[];
} fd_set;

#endif
