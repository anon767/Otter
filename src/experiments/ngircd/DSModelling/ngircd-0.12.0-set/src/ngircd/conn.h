/*
 * ngIRCd -- The Next Generation IRC Daemon
 * Copyright (c)2001,2002 by Alexander Barton (alex@barton.de)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * Please read the file COPYING, README and AUTHORS for more information.
 *
 * $Id: conn.h,v 1.46 2008/02/26 22:04:17 fw Exp $
 *
 * Connection management (header)
 */


#ifndef __conn_h__
#define __conn_h__


#include <time.h>			/* for time_t, see below */


#define CONN_ISCLOSING		1	/* Conn_Close() already called */
#define CONN_ISCONNECTING	2	/* connect() in progress */

#ifdef ZLIB
#define CONN_ZIP		4	/* zlib compressed link */
#endif


typedef int CONN_ID;

#include "client.h"

#ifdef CONN_MODULE

#include "defines.h"
#include "resolve.h"
#include "array.h"
#include "tool.h"
#include "ng_ipaddr.h"

#ifdef ZLIB
#include <zlib.h>
typedef struct _ZipData
{
	z_stream in;			/* "Handle" for input stream */
	z_stream out;			/* "Handle" for output stream */
	array rbuf;			/* Read buffer (compressed) */
	array wbuf;			/* Write buffer (uncompressed) */
	long bytes_in, bytes_out;	/* Counter for statistics (uncompressed!) */
} ZIPDATA;
#endif /* ZLIB */

typedef struct _Connection
{
	int sock;			/* Socket handle */
	ng_ipaddr_t addr;		/* Client address */
	RES_STAT res_stat;		/* Status of resolver process */
	char host[HOST_LEN];		/* Hostname */
	array rbuf;			/* Read buffer */
	array wbuf;			/* Write buffer */
	time_t signon;			/* Signon ("connect") time */
	time_t lastdata;		/* Last activity */
	time_t lastping;		/* Last PING */
	time_t lastprivmsg;		/* Last PRIVMSG */
	time_t delaytime;		/* Ignore link ("penalty") */
	long bytes_in, bytes_out;	/* Received and sent bytes */
	long msg_in, msg_out;		/* Received and sent IRC messages */
	int flag;			/* Flag (see "irc-write" module) */
	UINT16 options;			/* Link options / connection state */
	CLIENT *client;			/* pointer to client structure */
#ifdef ZLIB
	ZIPDATA zip;			/* Compression information */
#endif  /* ZLIB */
} CONNECTION;

GLOBAL CONNECTION *My_Connections;
GLOBAL CONN_ID Pool_Size;
GLOBAL long WCounter;

#endif /* CONN_MODULE */


GLOBAL void Conn_Init PARAMS((void ));
GLOBAL void Conn_Exit PARAMS(( void ));

GLOBAL unsigned int Conn_InitListeners PARAMS(( void ));
GLOBAL void Conn_ExitListeners PARAMS(( void ));

GLOBAL void Conn_Handler PARAMS(( void ));

GLOBAL bool Conn_WriteStr PARAMS(( CONN_ID Idx, char *Format, ... ));

GLOBAL void Conn_Close PARAMS(( CONN_ID Idx, char *LogMsg, char *FwdMsg, bool InformClient ));

GLOBAL void Conn_SyncServerStruct PARAMS(( void ));

GLOBAL CLIENT* Conn_GetClient PARAMS((CONN_ID i));
#endif

/* -eof- */
