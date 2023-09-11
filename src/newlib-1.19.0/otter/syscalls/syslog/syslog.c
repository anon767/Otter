/* adapted from uclibc */

/*
 * Copyright (c) 1983, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * SYSLOG -- print message on log file
 *
 * This routine looks a lot like printf, except that it outputs to the
 * log file instead of the standard output.  Also:
 *	adds a timestamp,
 *	prints the module name in front of the message,
 *	has some other formatting types (or will sometime),
 *	adds a newline on the end of the message.
 *
 * The output of this routine is intended to be read by syslogd(8).
 *
 * Author: Eric Allman
 * Modified to use UNIX domain IPC by Ralph Campbell
 * Patched March 12, 1996 by A. Ian Vogelesang <vogelesang@hdshq.com>
 *  - to correct the handling of message & format string truncation,
 *  - to visibly tag truncated records to facilitate
 *    investigation of such Bad Things with grep, and,
 *  - to correct the handling of case where "write"
 *    returns after writing only part of the message.
 * Rewritten by Martin Mares <mj@atrey.karlin.mff.cuni.cz> on May 14, 1997
 *  - better buffer overrun checks.
 *  - special handling of "%m" removed as we use GNU sprintf which handles
 *    it automatically.
 *  - Major code cleanup.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/syslog.h>

#include <sys/uio.h>
#include <sys/wait.h>
#include <netdb.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <paths.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>

/*#include <bits/uClibc_mutex.h>
__UCLIBC_MUTEX_STATIC(mylock, PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP);
*/

static int       LogFile = -1;          /* fd for log */
static unsigned short connected;             /* have done connect */
/* all bits in option argument for openlog() fit in 8 bits */
static unsigned short LogStat = 0;           /* status bits, set by openlog() */
static const char *LogTag = "syslog";   /* string to tag the entry with */
/* this fits in 8 bits too (LOG_LOCAL7 = 23<<3 = 184),
 * but NB: LOG_FACMASK is bigger (= 0x03f8 = 127<<3) for some strange reason.
 * Oh well. */
static int       LogFacility = LOG_USER;/* default facility code */
/* bits mask of priorities (eight prios - 8 bits is enough) */
static unsigned short LogMask = 0xff;        /* mask of priorities to be logged */
/* AF_UNIX address of local logger (we use struct sockaddr
 * instead of struct sockaddr_un since "/dev/log" is small enough) */
static const struct sockaddr SyslogAddr = {
	.sa_family = AF_UNIX, /* sa_family_t (usually a short) */
	.sa_data = _PATH_LOG  /* char [14] */
};

static void __otter_libc_closelog_intern(int sig)
{
	/* mylock must be held by the caller */
	if (LogFile != -1) {
		(void) close(LogFile);
	}
	LogFile = -1;
	connected = 0;
	if (sig == 0) { /* called from closelog()? - reset to defaults */
		LogStat = 0;
		LogTag = "syslog";
		LogFacility = LOG_USER;
		LogMask = 0xff;
	}
}

/*
 * OPENLOG -- open system log
 */
void openlog(const char *ident, int logstat, int logfac)
{
	/* nothing happens */
}

/*
 * syslog, vsyslog --
 *     print message on log file; output is intended for syslogd(8).
 */
void vsyslog(int pri, const char *fmt, va_list ap)
{
	/* data goes away */
}

void syslog(int pri, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyslog(pri, fmt, ap);
	va_end(ap);
}

/*
 * CLOSELOG -- close the system log
 */
void closelog(void)
{
	/*__UCLIBC_MUTEX_LOCK(mylock);*/
	__otter_libc_closelog_intern(0); /* 0: reset LogXXX globals to default */
	/*__UCLIBC_MUTEX_UNLOCK(mylock);*/
}

/* setlogmask -- set the log mask level */
int setlogmask(int pmask)
{
	int omask;

	omask = LogMask;
	if (pmask != 0) {
		/*__UCLIBC_MUTEX_LOCK(mylock)*/;
		LogMask = pmask;
		/*__UCLIBC_MUTEX_UNLOCK(mylock)*/;
	}
	return omask;
}

