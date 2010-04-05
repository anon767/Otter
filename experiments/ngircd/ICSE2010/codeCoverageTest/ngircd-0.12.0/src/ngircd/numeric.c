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
 * Handlers for IRC numerics sent to the server
 */

#include "portab.h"

static char UNUSED id[] = "$Id: numeric.c,v 1.1 2007/11/21 12:20:32 alex Exp $";

#include "imp.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defines.h"
#include "resolve.h"
#include "conn.h"
#include "conf.h"
#include "conn.h"
#include "client.h"
#include "channel.h"
#include "irc-write.h"
#include "lists.h"
#include "log.h"
#include "messages.h"
#include "parse.h"

#include "exp.h"
#include "numeric.h"


/**
 * Announce new server in the network
 * @param Client New server
 * @param Server Existing server in the network
 */
static bool
Announce_Server(CLIENT * Client, CLIENT * Server)
{
	CLIENT *c;

	if (Client_Conn(Server) > NONE) {
		/* Announce the new server to the one already registered
		 * which is directly connected to the local server */
		if (!IRC_WriteStrClient
		    (Server, "SERVER %s %d %d :%s", Client_ID(Client),
		     Client_Hops(Client) + 1, Client_MyToken(Client),
		     Client_Info(Client)))
			return DISCONNECTED;
	}

	if (Client_Hops(Server) == 1)
		c = Client_ThisServer();
	else
		c = Client_Introducer(Server);

	/* Inform new server about the one already registered in the network */
	return IRC_WriteStrClientPrefix(Client, c, "SERVER %s %d %d :%s",
		Client_ID(Server), Client_Hops(Server) + 1,
		Client_MyToken(Server), Client_Info(Server));
} /* Announce_Server */


/**
 * Announce existing user to a new server
 * @param Client New server
 * @param User Existing user in the network
 */
static bool
Announce_User(CLIENT * Client, CLIENT * User)
{
	return IRC_WriteStrClient(Client, "NICK %s %d %s %s %d +%s :%s",
		Client_ID(User), Client_Hops(User) + 1, Client_User(User),
		Client_Hostname(User), Client_MyToken(Client_Introducer(User)),
		Client_Modes(User), Client_Info(User));
} /* Announce_User */


#ifdef IRCPLUS

/**
 * Synchronize invite and ban lists between servers
 * @param Client New server
 */
static bool
Synchronize_Lists(CLIENT * Client)
{
	CHANNEL *c;
	struct list_head *head;
	struct list_elem *elem;

	assert(Client != NULL);

	c = Channel_First();
	while (c) {
		/* ban list */
		head = Channel_GetListBans(c);
		elem = Lists_GetFirst(head);
		while (elem) {
			if (!IRC_WriteStrClient(Client, "MODE %s +b %s",
						Channel_Name(c),
						Lists_GetMask(elem))) {
				return DISCONNECTED;
			}
			elem = Lists_GetNext(elem);
		}

		/* invite list */
		head = Channel_GetListInvites(c);
		elem = Lists_GetFirst(head);
		while (elem) {
			if (!IRC_WriteStrClient(Client, "MODE %s +I %s",
						Channel_Name(c),
						Lists_GetMask(elem))) {
				return DISCONNECTED;
			}
			elem = Lists_GetNext(elem);
		}

		c = Channel_Next(c);
	}
	return CONNECTED;
}


/**
 * Send CHANINFO commands to a new server (inform it about existing channels).
 * @param Client New server
 * @param Chan Channel
 */
static bool
Send_CHANINFO(CLIENT * Client, CHANNEL * Chan)
{
	char *modes, *topic;
	bool has_k, has_l;
	
#ifdef DEBUG
	Log(LOG_DEBUG, "Sending CHANINFO commands ...");
#endif
	
	modes = Channel_Modes(Chan);
	topic = Channel_Topic(Chan);
	
	if (!*modes && !*topic)
		return CONNECTED;
	
	has_k = strchr(modes, 'k') != NULL;
	has_l = strchr(modes, 'l') != NULL;
	
	/* send CHANINFO */
	if (!has_k && !has_l) {
		if (!*topic) {
			/* "CHANINFO <chan> +<modes>" */
			return IRC_WriteStrClient(Client, "CHANINFO %s +%s",
						  Channel_Name(Chan), modes);
		}
		/* "CHANINFO <chan> +<modes> :<topic>" */
		return IRC_WriteStrClient(Client, "CHANINFO %s +%s :%s",
					  Channel_Name(Chan), modes, topic);
	}
	/* "CHANINFO <chan> +<modes> <key> <limit> :<topic>" */
	return IRC_WriteStrClient(Client, "CHANINFO %s +%s %s %lu :%s",
				  Channel_Name(Chan), modes,
				  has_k ? Channel_Key(Chan) : "*",
				  has_l ? Channel_MaxUsers(Chan) : 0, topic);
} /* Send_CHANINFO */

#endif /* IRCPLUS */


/**
 * Handle ENDOFMOTD (376) numeric and login remote server.
 * The peer is either an IRC server (no IRC+ protocol), or we got the
 * ENDOFMOTD numeric from an IRC+ server. We have to register the new server.
 */
GLOBAL bool
IRC_Num_ENDOFMOTD(CLIENT * Client, UNUSED REQUEST * Req)
{
	char str[LINE_LEN];
	int max_hops, i;
	CLIENT *c, *cl;
	CHANNEL *chan;
	CL2CHAN *cl2chan;

	Client_SetType(Client, CLIENT_SERVER);

	Log(LOG_NOTICE | LOG_snotice,
	    "Server \"%s\" registered (connection %d, 1 hop - direct link).",
	    Client_ID(Client), Client_Conn(Client));

	/* Get highest hop count */
	max_hops = 0;
	c = Client_First();
	while (c) {
		if (Client_Hops(c) > max_hops)
			max_hops = Client_Hops(c);
		c = Client_Next(c);
	}

	/* Inform the new server about all other servers, and announce the
	 * new server to all the already registered ones. Important: we have
	 * to do this "in order" and can't introduce servers of which the
	 * "toplevel server" isn't known already. */
	for (i = 0; i < (max_hops + 1); i++) {
		for (c = Client_First(); c != NULL; c = Client_Next(c)) {
			if (Client_Type(c) != CLIENT_SERVER)
				continue;	/* not a server */
			if (Client_Hops(c) != i)
				continue;	/* not actual "nesting level" */
			if (c == Client || c == Client_ThisServer())
				continue;	/* that's us or the peer! */

			if (!Announce_Server(Client, c))
				return DISCONNECTED;
		}
	}

	/* Announce all the users to the new server */
	c = Client_First();
	while (c) {
		if (Client_Type(c) == CLIENT_USER) {
			if (!Announce_User(Client, c))
				return DISCONNECTED;
		}
		c = Client_Next(c);
	}

	/* Announce all channels to the new server */
	chan = Channel_First();
	while (chan) {
#ifdef IRCPLUS
		/* Send CHANINFO if the peer supports it */
		if (strchr(Client_Flags(Client), 'C')) {
			if (!Send_CHANINFO(Client, chan))
				return DISCONNECTED;
		}
#endif

		/* Get all the members of this channel */
		cl2chan = Channel_FirstMember(chan);
		snprintf(str, sizeof(str), "NJOIN %s :", Channel_Name(chan));
		while (cl2chan) {
			cl = Channel_GetClient(cl2chan);
			assert(cl != NULL);

			/* Nick name, with modes (if applicable) */
			if (str[strlen(str) - 1] != ':')
				strlcat(str, ",", sizeof(str));
			if (strchr(Channel_UserModes(chan, cl), 'v'))
				strlcat(str, "+", sizeof(str));
			if (strchr(Channel_UserModes(chan, cl), 'o'))
				strlcat(str, "@", sizeof(str));
			strlcat(str, Client_ID(cl), sizeof(str));

			/* Send the data if the buffer is "full" */
			if (strlen(str) > (LINE_LEN - CLIENT_NICK_LEN - 8)) {
				if (!IRC_WriteStrClient(Client, "%s", str))
					return DISCONNECTED;
				snprintf(str, sizeof(str), "NJOIN %s :",
					 Channel_Name(chan));
			}

			cl2chan = Channel_NextMember(chan, cl2chan);
		}

		/* Data left in the buffer? */
		if (str[strlen(str) - 1] != ':') {
			/* Yes, send it ... */
			if (!IRC_WriteStrClient(Client, "%s", str))
				return DISCONNECTED;
		}

		/* Get next channel ... */
		chan = Channel_Next(chan);
	}

#ifdef IRCPLUS
	if (strchr(Client_Flags(Client), 'L')) {
		LogDebug("Synchronizing INVITE- and BAN-lists ...");
		if (!Synchronize_Lists(Client))
			return DISCONNECTED;
	}
#endif

	return CONNECTED;
} /* IRC_Num_ENDOFMOTD */


/**
 * Handle ISUPPORT (005) numeric.
 */
GLOBAL bool
IRC_Num_ISUPPORT(CLIENT * Client, REQUEST * Req)
{
	int i;
	char *key, *value;

	for (i = 1; i < Req->argc - 1; i++) {
		key = Req->argv[i];
		value = strchr(key, '=');
		if (value)
			*value++ = '\0';
		else
			value = "";

		if (strcmp("NICKLEN", key) == 0) {
			if ((unsigned int)atol(value) == Conf_MaxNickLength - 1)
				continue;

			/* Nick name length settings are different! */
			Log(LOG_ERR,
			    "Peer uses incompatible nick name length (%d/%d)! Disconnecting ...",
			    Conf_MaxNickLength - 1, atoi(value));
			Conn_Close(Client_Conn(Client),
				   "Incompatible nick name length",
				   NULL, false);
			return DISCONNECTED;
		}
	}

	return CONNECTED;
} /* IRC_Num_ISUPPORT */


/* -eof- */
