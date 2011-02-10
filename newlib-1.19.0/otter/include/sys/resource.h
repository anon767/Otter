#ifndef _OTTER_SYS_RESOURCE_H_
#define	_OTTER_SYS_RESOURCE_H_

/*****
 * PRIORITY
 */

// Include the one in newlib/libc
#include_next <sys/resource.h>

/*
 * Possible values of the first parameter to getpriority()/setpriority(),
 * used to indicate the type of the second parameter.
 */
#define	PRIO_PROCESS	0		/* Second argument is a PID */
#define	PRIO_PGRP	1		/* Second argument is a GID */
#define	PRIO_USER	2		/* Second argument is a UID */

/*
 * Range limitations for the value of the third parameter to setpriority().
 */
#define	PRIO_MIN	-20
#define	PRIO_MAX	20

/* id_t is supposed to be defined in sys/types.h
 * But newlib's sys/types.h does not have it.
 */
typedef int id_t;

/* Otter specific mocking */
#define _SYS_RESOURCE_MAX_ID 2     
/* 0 - PRIO_PROCESS, 1 - PRIO_PGRP, 2 - PRIO_USER */
struct { int prio; int has_set; } _SYS_RESOURCE_PRIORITY_TABLE[3][_SYS_RESOURCE_MAX_ID];

int getpriority(int which, id_t who);
int setpriority(int which, id_t who, int prio);

#endif	/* !_SYS_RESOURCE_H_ */
