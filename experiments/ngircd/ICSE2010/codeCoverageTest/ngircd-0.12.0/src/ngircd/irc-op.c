/*
 * ngIRCd -- The Next Generation IRC Daemon
 * Copyright (c)2001-2005 by Alexander Barton (alex@barton.de)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * Please read the file COPYING, README and AUTHORS for more information.
 *
 * Channel operator commands
 */


#include "portab.h"

static char UNUSED id[] = "$Id: irc-op.c,v 1.17 2006/12/07 17:57:20 fw Exp $";

#include "imp.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "defines.h"
#include "conn.h"
#include "client.h"
#include "channel.h"
#include "irc-write.h"
#include "lists.h"
#include "log.h"
#include "messages.h"
#include "parse.h"

#include "exp.h"
#include "irc-op.h"


GLOBAL bool
IRC_KICK( CLIENT *Client, REQUEST *Req )
{
	CLIENT *target, *from;
	
	assert( Client != NULL );
	assert( Req != NULL );

	/* Falsche Anzahl Parameter? */
	if(( Req->argc < 2) || ( Req->argc > 3 )) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

	if( Client_Type( Client ) == CLIENT_SERVER ) from = Client_Search( Req->prefix );
	else from = Client;
	if( ! from ) return IRC_WriteStrClient( Client, ERR_NOSUCHNICK_MSG, Client_ID( Client ), Req->prefix );
	
	/* Ziel-User suchen */
	target = Client_Search( Req->argv[1] );
	if( ! target ) return IRC_WriteStrClient( from, ERR_NOSUCHNICK_MSG, Client_ID( from ), Req->argv[1] );

	Channel_Kick( target, from, Req->argv[0], Req->argc == 3 ? Req->argv[2] : Client_ID( from ));
	return CONNECTED;
} /* IRC_KICK */	


GLOBAL bool
IRC_INVITE( CLIENT *Client, REQUEST *Req )
{
	CHANNEL *chan;
	CLIENT *target, *from;
	bool remember = false;

	assert( Client != NULL );
	assert( Req != NULL );

	/* Wrong number of parameters? */
	if( Req->argc != 2 ) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

	if( Client_Type( Client ) == CLIENT_SERVER ) from = Client_Search( Req->prefix );
	else from = Client;
	if( ! from ) return IRC_WriteStrClient( Client, ERR_NOSUCHNICK_MSG, Client_ID( Client ), Req->prefix );
	
	/* Search user */
	target = Client_Search( Req->argv[0] );
	if(( ! target ) || ( Client_Type( target ) != CLIENT_USER )) return IRC_WriteStrClient( from, ERR_NOSUCHNICK_MSG, Client_ID( Client ), Req->argv[0] );

	chan = Channel_Search( Req->argv[1] );

	if( chan )
	{
		/* Channel exists. Is the user a valid member of the channel? */
		if( ! Channel_IsMemberOf( chan, from )) return IRC_WriteStrClient( from, ERR_NOTONCHANNEL_MSG, Client_ID( Client ), Req->argv[1] );

		/* Is the channel "invite-only"? */
		if( strchr( Channel_Modes( chan ), 'i' ))
		{
			/* Yes. The user must be channel operator! */
			if( ! strchr( Channel_UserModes( chan, from ), 'o' )) return IRC_WriteStrClient( from, ERR_CHANOPRIVSNEEDED_MSG, Client_ID( from ), Channel_Name( chan ));
			remember = true;
		}

		/* Is the target user already member of the channel? */
		if( Channel_IsMemberOf( chan, target )) return IRC_WriteStrClient( from, ERR_USERONCHANNEL_MSG, Client_ID( from ), Req->argv[0], Req->argv[1] );

		/* If the target user is banned on that channel: remember invite */
		if( Lists_Check(Channel_GetListBans(chan), target )) remember = true;

		if (remember) {
			/* We must remember this invite */
			if( ! Channel_AddInvite(chan, Client_Mask( target ), true))
				return CONNECTED;
		}
	}

	LogDebug("User \"%s\" invites \"%s\" to \"%s\" ...", Client_Mask(from), Req->argv[0], Req->argv[1]);

	/* Inform target client */
	IRC_WriteStrClientPrefix( target, from, "INVITE %s %s", Req->argv[0], Req->argv[1] );

	if( Client_Conn( target ) > NONE )
	{
		/* The target user is local, so we have to send the status code */
		if( ! IRC_WriteStrClientPrefix( from, target, RPL_INVITING_MSG, Client_ID( from ), Req->argv[0], Req->argv[1] )) return DISCONNECTED;
	}
	
	return CONNECTED;
} /* IRC_INVITE */


/* -eof- */
