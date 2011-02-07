#ifndef _OTTER_SYS_RESOURCE_H_
#define	_OTTER_SYS_RESOURCE_H_

/*****
 * PRIORITY
 */

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

#include_next <sys/resource.h>

#endif	/* !_SYS_RESOURCE_H_ */
