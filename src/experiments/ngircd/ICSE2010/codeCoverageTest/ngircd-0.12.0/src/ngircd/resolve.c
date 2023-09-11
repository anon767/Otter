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
 * Asynchronous resolver
 */


#include "portab.h"

static char UNUSED id[] = "$Id: resolve.c,v 1.29 2008/02/26 22:04:17 fw Exp $";

#include "imp.h"
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#ifdef IDENTAUTH
#ifdef HAVE_IDENT_H
#include <ident.h>
#endif
#endif

#include "conn.h"
#include "defines.h"
#include "log.h"

#include "exp.h"
#include "resolve.h"
#include "io.h"


static void Do_ResolveAddr PARAMS(( const ng_ipaddr_t *Addr, int Sock, int w_fd ));
static void Do_ResolveName PARAMS(( const char *Host, int w_fd ));
static bool register_callback PARAMS((RES_STAT *s, void (*cbfunc)(int, short)));

#ifdef WANT_IPV6
extern bool Conf_ConnectIPv4;
extern bool Conf_ConnectIPv6;
#endif

static pid_t
Resolver_fork(int *pipefds)
{
	pid_t pid;

	if (pipe(pipefds) != 0) {
                Log( LOG_ALERT, "Resolver: Can't create output pipe: %s!", strerror( errno ));
                return -1;
	}

	pid = fork();
	switch(pid) {
		case -1:
			Log( LOG_CRIT, "Resolver: Can't fork: %s!", strerror( errno ));
			close(pipefds[0]);
			close(pipefds[1]);
			return -1;
		case 0: /* child */
			close(pipefds[0]);
			Log_Init_Resolver( );
			return 0;
	}
	/* parent */
	close(pipefds[1]);
	return pid; 
}


/**
 * Resolve IP (asynchronous!).
 */
GLOBAL bool
Resolve_Addr(RES_STAT * s, const ng_ipaddr_t *Addr, int identsock,
	     void (*cbfunc) (int, short))
{
	int pipefd[2];
	pid_t pid;

	assert(s != NULL);

	pid = Resolver_fork(pipefd);
	if (pid > 0) {
		Log(LOG_DEBUG, "Resolver for %s created (PID %d).", ng_ipaddr_tostr(Addr), pid);

		s->pid = pid;
		s->resolver_fd = pipefd[0];
		return register_callback(s, cbfunc);
	} else if( pid == 0 ) {
		/* Sub process */
		Do_ResolveAddr( Addr, identsock, pipefd[1]);
		Log_Exit_Resolver( );
		exit(0);
	}
	return false;
} /* Resolve_Addr */


/**
 * Resolve hostname (asynchronous!).
 */
GLOBAL bool
Resolve_Name( RES_STAT *s, const char *Host, void (*cbfunc)(int, short))
{
	int pipefd[2];
	pid_t pid;

	assert(s != NULL);

	pid = Resolver_fork(pipefd);
	if (pid > 0) {
		/* Main process */
#ifdef DEBUG
		Log( LOG_DEBUG, "Resolver for \"%s\" created (PID %d).", Host, pid );
#endif
		s->pid = pid;
		s->resolver_fd = pipefd[0];
		return register_callback(s, cbfunc);
	} else if( pid == 0 ) {
		/* Sub process */
		Do_ResolveName(Host, pipefd[1]);
		Log_Exit_Resolver( );
		exit(0);
	}
	return false;
} /* Resolve_Name */


GLOBAL void
Resolve_Init(RES_STAT *s)
{
	assert(s != NULL);
	s->resolver_fd = -1;
	s->pid = 0;
}


#ifndef WANT_IPV6
#ifdef h_errno
static char *
Get_Error( int H_Error )
{
	/* Get error message for H_Error */
	switch( H_Error ) {
	case HOST_NOT_FOUND:
		return "host not found";
	case NO_DATA:
		return "name valid but no IP address defined";
	case NO_RECOVERY:
		return "name server error";
	case TRY_AGAIN:
		return "name server temporary not available";
	}
	return "unknown error";
}
#endif /* h_errno */
#endif /* WANT_IPV6 */


/* Do "IDENT" (aka "AUTH") lookup and append result to resolved_addr array */
static void
Do_IdentQuery(int identsock, array *resolved_addr)
{
#ifdef IDENTAUTH
	char *res;

	assert(identsock >= 0);

#ifdef DEBUG
	Log_Resolver(LOG_DEBUG, "Doing IDENT lookup on socket %d ...", identsock);
#endif
	if (identsock < 0)
		return;
	res = ident_id( identsock, 10 );
#ifdef DEBUG
	Log_Resolver(LOG_DEBUG, "Ok, IDENT lookup on socket %d done: \"%s\"",
						identsock, res ? res : "(NULL)" );
#endif
	if (!res) /* no result */
		return;
	if (!array_cats(resolved_addr, res))
		Log_Resolver(LOG_WARNING, "Resolver: Cannot copy IDENT result: %s!", strerror(errno));

	free(res);
#else
	(void) identsock;
	(void) resolved_addr;
#endif
}


/**
 * perform reverse DNS lookup and put result string into resbuf.
 * If no hostname could be obtained, this function stores the string representation of
 * the IP address in resbuf and returns false.
 * @param IpAddr ip address to resolve
 * @param resbuf result buffer to store DNS name/string representation of ip address
 * @reslen size of result buffer (must be >= NGT_INET_ADDRSTRLEN)
 * @return true if reverse lookup successful, false otherwise
 */
static bool
ReverseLookup(const ng_ipaddr_t *IpAddr, char *resbuf, size_t reslen)
{
	char tmp_ip_str[NG_INET_ADDRSTRLEN];
	const char *errmsg;
#ifdef HAVE_GETNAMEINFO
	static const char funcname[]="getnameinfo";
	int res;

	*resbuf = 0;

	res = getnameinfo((struct sockaddr *) IpAddr, ng_ipaddr_salen(IpAddr),
				resbuf, reslen, NULL, 0, NI_NAMEREQD);
	if (res == 0)
		return true;

	if (res == EAI_SYSTEM)
		errmsg = strerror(errno);
	else
		errmsg = gai_strerror(res);
#else
	const struct sockaddr_in *Addr = (const struct sockaddr_in *) IpAddr;
	struct hostent *h;
	static const char funcname[]="gethostbyaddr";

	h = gethostbyaddr((char *)&Addr->sin_addr, sizeof(Addr->sin_addr), AF_INET);
	if (h) {
		if (strlcpy(resbuf, h->h_name, reslen) < reslen)
			return true;
		errmsg = "hostname too long";
	} else {
# ifdef h_errno
		errmsg = Get_Error(h_errno);
# else
		errmsg = "unknown error";
# endif /* h_errno */
	}
#endif	/* HAVE_GETNAMEINFO */

	assert(errmsg);
	assert(reslen >= NG_INET_ADDRSTRLEN);
	ng_ipaddr_tostr_r(IpAddr, tmp_ip_str);

	Log_Resolver(LOG_WARNING, "%s: Can't resolve address \"%s\": %s",
				funcname, tmp_ip_str, errmsg);
	strlcpy(resbuf, tmp_ip_str, reslen);
	return false;
}


/**
 * perform DNS lookup of given host name and fill IpAddr with a list of
 * ip addresses associated with that name.
 * ip addresses found are stored in the "array *IpAddr" argument (type ng_ipaddr_t)
 * @param hostname The domain name to look up.
 * @param IpAddr pointer to empty and initialized array to store results
 * @return true if lookup successful, false if domain name not found
 */
static bool
ForwardLookup(const char *hostname, array *IpAddr)
{
	ng_ipaddr_t addr;
#ifdef HAVE_GETADDRINFO
	int res;
	struct addrinfo *a, *ai_results;
	static struct addrinfo hints = {
#ifndef WANT_IPV6
		.ai_family = AF_INET,
#endif
#ifdef AI_ADDRCONFIG	/* glibc has this, but not e.g. netbsd 4.0 */
		.ai_flags = AI_ADDRCONFIG,
#endif
		.ai_socktype = SOCK_STREAM,
		.ai_protocol = IPPROTO_TCP
	};
#ifdef WANT_IPV6
	assert(Conf_ConnectIPv6 || Conf_ConnectIPv4);

	if (!Conf_ConnectIPv6)
		hints.ai_family = AF_INET;
	if (!Conf_ConnectIPv4)
		hints.ai_family = AF_INET6;
#endif
	res = getaddrinfo(hostname, NULL, &hints, &ai_results);
	switch (res) {
	case 0:	break;
	case EAI_SYSTEM:
		Log_Resolver(LOG_WARNING, "Can't resolve \"%s\": %s", hostname, strerror(errno));
		return false;
	default:
		Log_Resolver(LOG_WARNING, "Can't resolve \"%s\": %s", hostname, gai_strerror(res));
		return false;
	}

	for (a = ai_results; a != NULL; a = a->ai_next) {
		assert(a->ai_addrlen <= sizeof(addr));

		if (a->ai_addrlen > sizeof(addr))
			continue;

		memcpy(&addr, a->ai_addr, a->ai_addrlen);

		if (!array_catb(IpAddr, (char *)&addr, sizeof(addr)))
			break;
	}

	freeaddrinfo(ai_results);
	return a == NULL;
#else
	struct hostent *h = gethostbyname(hostname);

	if (!h) {
#ifdef h_errno
		Log_Resolver(LOG_WARNING, "Can't resolve \"%s\": %s", hostname, Get_Error(h_errno));
#else
		Log_Resolver(LOG_WARNING, "Can't resolve \"%s\"", hostname);
#endif
		return false;
	}
	memset(&addr, 0, sizeof(addr));

	addr.sin4.sin_family = AF_INET;
	memcpy(&addr.sin4.sin_addr, h->h_addr, sizeof(struct in_addr));

	return array_copyb(IpAddr, (char *)&addr, sizeof(addr));
#endif /* HAVE_GETADDRINFO */
}


static bool
Addr_in_list(const array *resolved_addr, const ng_ipaddr_t *Addr)
{
	char tmp_ip_str[NG_INET_ADDRSTRLEN];
	const ng_ipaddr_t *tmpAddrs = array_start(resolved_addr);
	size_t len = array_length(resolved_addr, sizeof(*tmpAddrs));

	assert(len > 0);
	assert(tmpAddrs);

	while (len > 0) {
		if (ng_ipaddr_ipequal(Addr, tmpAddrs))
			return true;
		tmpAddrs++;
		len--;
	}
	/* failed; print list of addresses */
	ng_ipaddr_tostr_r(Addr, tmp_ip_str);
	len = array_length(resolved_addr, sizeof(*tmpAddrs));
	tmpAddrs = array_start(resolved_addr);

	while (len > 0) {
		Log_Resolver(LOG_WARNING, "Address mismatch: %s != %s",
			tmp_ip_str, ng_ipaddr_tostr(tmpAddrs));
		tmpAddrs++;
		len--;
	}

	return false;
}


static void
Log_Forgery_NoIP(const char *ip, const char *host)
{
	Log_Resolver(LOG_WARNING, "Possible forgery: %s resolved to %s "
		"(which has no ip address)", ip, host);
}

static void
Log_Forgery_WrongIP(const char *ip, const char *host)
{
	Log_Resolver(LOG_WARNING,"Possible forgery: %s resolved to %s "
		"(which points to different address)", ip, host);
}


static void
ArrayWrite(int fd, const array *a)
{
	size_t len = array_bytes(a);
	const char *data = array_start(a);

	assert(data);

	if( (size_t)write(fd, data, len) != len )
		Log_Resolver( LOG_CRIT, "Resolver: Can't write to parent: %s!",
							strerror(errno));
}


static void
Do_ResolveAddr(const ng_ipaddr_t *Addr, int identsock, int w_fd)
{
	/* Resolver sub-process: resolve IP address and write result into
	 * pipe to parent. */
	char hostname[CLIENT_HOST_LEN];
	char tmp_ip_str[NG_INET_ADDRSTRLEN];
	size_t len;
	array resolved_addr;

	array_init(&resolved_addr);
	ng_ipaddr_tostr_r(Addr, tmp_ip_str);
#ifdef DEBUG
	Log_Resolver(LOG_DEBUG, "Now resolving %s ...", tmp_ip_str);
#endif
	if (!ReverseLookup(Addr, hostname, sizeof(hostname)))
		goto dns_done;

	if (ForwardLookup(hostname, &resolved_addr)) {
		if (!Addr_in_list(&resolved_addr, Addr)) {
			Log_Forgery_WrongIP(tmp_ip_str, hostname);
			strlcpy(hostname, tmp_ip_str, sizeof(hostname));
		}
	} else {
		Log_Forgery_NoIP(tmp_ip_str, hostname);
		strlcpy(hostname, tmp_ip_str, sizeof(hostname));
	}
#ifdef DEBUG
	Log_Resolver(LOG_DEBUG, "Ok, translated %s to \"%s\".", tmp_ip_str, hostname);
#endif
 dns_done:
	len = strlen(hostname);
	hostname[len] = '\n';
	if (!array_copyb(&resolved_addr, hostname, ++len)) {
		Log_Resolver(LOG_CRIT, "Resolver: Can't copy resolved name: %s!", strerror(errno));
		array_free(&resolved_addr);
		return;
	}

	Do_IdentQuery(identsock, &resolved_addr);

	ArrayWrite(w_fd, &resolved_addr);

	array_free(&resolved_addr);
} /* Do_ResolveAddr */


static void
Do_ResolveName( const char *Host, int w_fd )
{
	/* Resolver sub-process: resolve name and write result into pipe
	 * to parent. */
	array IpAddrs;
#ifdef DEBUG
	ng_ipaddr_t *addr;
	size_t len;
#endif
	Log_Resolver(LOG_DEBUG, "Now resolving \"%s\" ...", Host);

	array_init(&IpAddrs);
	/* Resolve hostname */
	if (!ForwardLookup(Host, &IpAddrs)) {
		close(w_fd);
		return;
	}
#ifdef DEBUG
	len = array_length(&IpAddrs, sizeof(*addr));
	assert(len > 0);
	addr = array_start(&IpAddrs);
	assert(addr);
	for (; len > 0; --len,addr++) {
		Log_Resolver(LOG_DEBUG, "translated \"%s\" to %s.",
					Host, ng_ipaddr_tostr(addr));
	}
#endif
	/* Write result into pipe to parent */
	ArrayWrite(w_fd, &IpAddrs);

	array_free(&IpAddrs);
} /* Do_ResolveName */


static bool
register_callback( RES_STAT *s, void (*cbfunc)(int, short))
{
	assert(cbfunc != NULL);
	assert(s != NULL);
	assert(s->resolver_fd >= 0);

	if (io_setnonblock(s->resolver_fd) &&
		io_event_create(s->resolver_fd, IO_WANTREAD, cbfunc))
			return true;

	Log( LOG_CRIT, "Resolver: Could not register callback function: %s!", strerror(errno));
	close(s->resolver_fd);
	Resolve_Init(s);
	return false;
}


GLOBAL bool
Resolve_Shutdown( RES_STAT *s)
{
	bool ret = false;

	assert(s != NULL);
	assert(s->resolver_fd >= 0);

	if (s->resolver_fd >= 0)
		ret = io_close(s->resolver_fd);

	Resolve_Init(s);
	return ret;
}


/**
 * Read result of resolver sub-process from pipe
 */
GLOBAL size_t
Resolve_Read( RES_STAT *s, void* readbuf, size_t buflen)
{
	ssize_t bytes_read;

	assert(buflen > 0);

	/* Read result from pipe */
	bytes_read = read(s->resolver_fd, readbuf, buflen);
	if (bytes_read < 0) {
		if (errno == EAGAIN)
			return 0;

		Log( LOG_CRIT, "Resolver: Can't read result: %s!", strerror(errno));
		bytes_read = 0;
	}
#ifdef DEBUG
	else if (bytes_read == 0)
		Log( LOG_DEBUG, "Resolver: Can't read result: EOF");
#endif
	Resolve_Shutdown(s);
	return (size_t)bytes_read;
}
/* -eof- */

