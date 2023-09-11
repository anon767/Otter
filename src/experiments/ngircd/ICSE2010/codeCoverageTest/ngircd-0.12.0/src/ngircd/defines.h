/*
 * ngIRCd -- The Next Generation IRC Daemon
 * Copyright (c)2001-2007 Alexander Barton (alex@barton.de)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * Please read the file COPYING, README and AUTHORS for more information.
 *
 * $Id: defines.h,v 1.62 2007/11/21 12:16:36 alex Exp $
 */


#ifndef __defines_h__
#define __defines_h__

/**
 * @file
 * Global constants ("#defines") used by the ngIRCd.
 */

#define NONE -1

#define FNAME_LEN 256			/* Max. length of file name */

#define LINE_LEN 256			/* Max. length of a line in the
					   configuration file */

#define HOST_LEN 256			/* Max. lenght of fully qualified host
					   names (e. g. "abc.domain.tld") */

#define MAX_LISTEN_PORTS 16		/* Max. count of listening ports */

#define MAX_OPERATORS 16		/* Max. count of configurable IRC Ops */

#define MAX_SERVERS 16			/* Max. count of configurable servers */

#define MAX_DEFCHANNELS 16		/* Max. count of predefined channels */

#define MAX_SERVICES 8			/* Max. count of services */

#define MAX_WHOWAS 64			/* Max. number of WHOWAS items */
#define DEFAULT_WHOWAS 5		/* default count for WHOWAS command */

#define CONNECTION_POOL 100		/* Size of default connection pool */

#define CLIENT_ID_LEN 64		/* Max. length of an IRC ID; see RFC
					   RFC 2812 section 1.1 and 1.2.1 */
#define CLIENT_NICK_LEN_DEFAULT 10	/* Default nick length, see. RFC 2812
					 * section 1.2.1 */
#define CLIENT_NICK_LEN 32		/* Maximum nick name length */
#define CLIENT_PASS_LEN 21		/* Max. password length */
#define CLIENT_USER_LEN 10		/* Max. length of user name ("login")
					   see RFC 2812, section 1.2.1 */
#define CLIENT_NAME_LEN 32		/* Max. length of "real names" */
#define CLIENT_HOST_LEN 64		/* Max. host name length */
#define CLIENT_MODE_LEN 9		/* Max. lenth of all client modes */
#define CLIENT_INFO_LEN 64		/* Max. length of server info texts */
#define CLIENT_AWAY_LEN 128		/* Max. length of away messages */
#define CLIENT_FLAGS_LEN 100		/* Max. length of client flags */

#define CHANNEL_NAME_LEN 51		/* Max. length of a channel name, see
					   RFC 2812 section 1.3 */
#define CHANNEL_MODE_LEN 9		/* Max. length of channel modes */

#define COMMAND_LEN 513			/* Max. IRC command length, see. RFC
					   2812 section 3.2 */

#define READBUFFER_LEN 2048		/* Size of the read buffer of a
					   connection in bytes. */
#define WRITEBUFFER_LEN 4096		/* Size of the write buffer of a
					   connection in bytes. */
#define WRITEBUFFER_SLINK_LEN 51200	/* Size of the write buffer of a
					   server link connection in bytes. */

#define PROTOVER "0210"			/* Implemented IRC protocol version,
					   see RFC 2813 section 4.1.1. */
#define PROTOIRC "-IRC"			/* Protocol suffix, see RFC 2813
					   section 4.1.1 */
#define PROTOIRCPLUS "-IRC+"		/* Protocol suffix used by the IRC+
					   protocol, see doc/Protocol.txt */

#ifdef IRCPLUS
# define IRCPLUSFLAGS "CHL"		/* Standard IRC+ flags */
#endif

#define STARTUP_DELAY 1			/* Delay outgoing connections n seconds
					   after startup. */
#define RECONNECT_DELAY 3		/* Time to delay re-connect attempts
					   in seconds. */

#define USERMODES "aios"		/* Supported user modes. */
#define CHANMODES "biIklmnoPstv"	/* Supported channel modes. */

#define CONNECTED true			/* Internal status codes. */
#define DISCONNECTED false

#define DEFAULT_AWAY_MSG "Away"		/* Away message for users connected to
					   linked servers. */

#define DEFAULT_TOPIC_ID "-Server-"	/* Default ID for "topic owner". */

#define CONFIG_FILE "/ngircd.conf"	/* Configuration file name. */
#define MOTD_FILE "/ngircd.motd"	/* Name of the MOTD file. */
#define MOTD_PHRASE ""			/* Default MOTD phrase string. */
#define CHROOT_DIR ""			/* Default chroot() directory. */
#define PID_FILE ""			/* Default file for the process ID. */

#define ERROR_DIR "/tmp"		/* Error directory used in debug mode */

#define MAX_LOG_MSG_LEN 256		/* Max. length of a log message. */

#define TOKEN_OUTBOUND -2		/* Tag for outbound server links. */

#define NOTICE_TXTPREFIX ""		/* Prefix for NOTICEs from the server
					   to users. Some servers use '*'. */

#define CUT_TXTSUFFIX "[CUT]"		/* Suffix for oversized messages that
					   have been shortened and cut off. */

#ifdef ZEROCONF
#define MDNS_TYPE "_ircu._tcp."		/* Service type to register with mDNS */
#endif


#endif


/* -eof- */
