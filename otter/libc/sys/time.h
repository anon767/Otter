#ifndef _SYS_TIME_H
#define _SYS_TIME_H

#include <sys/types.h>

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
	long fds_bits[];
} fd_set;

int   getitimer(int, struct itimerval *);
int   setitimer(int, const struct itimerval *, struct itimerval *);
int   gettimeofday(struct timeval *, void *);
int   select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
int   utimes(const char *, const struct timeval [2]);

#endif
