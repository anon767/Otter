/*
 * ngIRCd -- The Next Generation IRC Daemon
 * Copyright (c)2001-2003 by Alexander Barton (alex@barton.de)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * Please read the file COPYING, README and AUTHORS for more information.
 *
 * $Id: resolve.h,v 1.14 2008/02/26 22:04:17 fw Exp $
 *
 * Asynchronous resolver (header)
 */


#ifndef __resolve_h__
#define __resolve_h__

#include "array.h"
#include "tool.h"
#include "ng_ipaddr.h"
#include <netinet/in.h>

/* This struct must not be accessed directly */
typedef struct _Res_Stat {
	pid_t pid;			/* PID of resolver process */
	int resolver_fd;		/* pipe fd for lookup result. */
} RES_STAT;


#define Resolve_Getfd(x)		((x)->resolver_fd)
#define Resolve_INPROGRESS(x)		((x)->resolver_fd >= 0)

GLOBAL bool Resolve_Addr PARAMS(( RES_STAT *s, const ng_ipaddr_t *Addr, int identsock, void (*cbfunc)(int, short)));
GLOBAL bool Resolve_Name PARAMS(( RES_STAT *s, const char *Host, void (*cbfunc)(int, short) ));
GLOBAL size_t Resolve_Read PARAMS(( RES_STAT *s, void *buf, size_t buflen));
GLOBAL void Resolve_Init PARAMS(( RES_STAT *s));
GLOBAL bool Resolve_Shutdown PARAMS(( RES_STAT *s));

#endif
/* -eof- */
