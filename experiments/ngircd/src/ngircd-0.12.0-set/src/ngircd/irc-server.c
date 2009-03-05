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
 * IRC commands for server links
 */


#include "portab.h"

static char UNUSED id[] = "$Id: irc-server.c,v 1.46 2007/11/21 12:16:36 alex Exp $";

#include "imp.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "defines.h"
#include "resolve.h"
#include "conn.h"
#include "conn-zip.h"
#include "conf.h"
#include "client.h"
#include "channel.h"
#include "irc-write.h"
#include "lists.h"
#include "log.h"
#include "messages.h"
#include "parse.h"
#include "numeric.h"
#include "ngircd.h"
#include "irc-info.h"

#include "exp.h"
#include "irc-server.h"


/**
 * Handler for the IRC command "SERVER".
 * See RFC 2813 section 4.1.2.
 */
GLOBAL bool
IRC_SERVER( CLIENT *Client, REQUEST *Req )
{
	char str[LINE_LEN], *ptr;
	CLIENT *from, *c;
	bool ok;
	int i;
	CONN_ID con;
	
	assert( Client != NULL );
	assert( Req != NULL );

	/* Return an error if this is not a local client */
	if (Client_Conn(Client) <= NONE)
		return IRC_WriteStrClient(Client, ERR_UNKNOWNCOMMAND_MSG,
					  Client_ID(Client), Req->command);

	if (Client_Type(Client) == CLIENT_GOTPASS) {
		/* We got a PASS command from the peer, and now a SERVER
		 * command: the peer tries to register itself as a server. */
		LogDebug("Connection %d: got SERVER command (new server link) ...",
			Client_Conn(Client));

		/* Falsche Anzahl Parameter? */
		if(( Req->argc != 2 ) && ( Req->argc != 3 )) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

		/* Ist dieser Server bei uns konfiguriert? */
		for( i = 0; i < MAX_SERVERS; i++ ) if( strcasecmp( Req->argv[0], Conf_Server[i].name ) == 0 ) break;
		if( i >= MAX_SERVERS )
		{
			/* Server ist nicht konfiguriert! */
			Log( LOG_ERR, "Connection %d: Server \"%s\" not configured here!", Client_Conn( Client ), Req->argv[0] );
			Conn_Close( Client_Conn( Client ), NULL, "Server not configured here", true);
			return DISCONNECTED;
		}
		if( strcmp( Client_Password( Client ), Conf_Server[i].pwd_in ) != 0 )
		{
			/* Falsches Passwort */
			Log( LOG_ERR, "Connection %d: Got bad password from server \"%s\"!", Client_Conn( Client ), Req->argv[0] );
			Conn_Close( Client_Conn( Client ), NULL, "Bad password", true);
			return DISCONNECTED;
		}
		
		/* Ist ein Server mit dieser ID bereits registriert? */
		if( ! Client_CheckID( Client, Req->argv[0] )) return DISCONNECTED;

		/* Server-Strukturen fuellen ;-) */
		Client_SetID( Client, Req->argv[0] );
		Client_SetHops( Client, 1 );
		Client_SetInfo( Client, Req->argv[Req->argc - 1] );

		/* Meldet sich der Server bei uns an (d.h., bauen nicht wir
		 * selber die Verbindung zu einem anderen Server auf)? */
		con = Client_Conn( Client );
		if( Client_Token( Client ) != TOKEN_OUTBOUND )
		{
			/* Eingehende Verbindung: Unseren SERVER- und PASS-Befehl senden */
			ok = true;
			if( ! IRC_WriteStrClient( Client, "PASS %s %s", Conf_Server[i].pwd_out, NGIRCd_ProtoID )) ok = false;
			else ok = IRC_WriteStrClient( Client, "SERVER %s 1 :%s", Conf_ServerName, Conf_ServerInfo );
			if( ! ok )
			{
				Conn_Close( con, "Unexpected server behavior!", NULL, false );
				return DISCONNECTED;
			}
			Client_SetIntroducer( Client, Client );
			Client_SetToken( Client, 1 );
		}
		else
		{
			/* Ausgehende verbindung, SERVER und PASS wurden von uns bereits
			 * an die Gegenseite uerbermittelt */
			Client_SetToken( Client, atoi( Req->argv[1] ));
		}

		/* Mark this connection as belonging to an configured server */
		Conf_SetServer(i, con);
		
		Client_SetType(Client, CLIENT_UNKNOWNSERVER);

#ifdef ZLIB
		/* Kompression initialisieren, wenn erforderlich */
		if( strchr( Client_Flags( Client ), 'Z' ))
		{
			if( ! Zip_InitConn( con ))
			{
				/* Fehler! */
				Conn_Close( con, "Can't inizialize compression (zlib)!", NULL, false );
				return DISCONNECTED;
			}
		}
#endif

#ifdef IRCPLUS
		if (strchr(Client_Flags(Client), 'H')) {
			LogDebug("Peer supports IRC+ extended server handshake ...");
			if (!IRC_Send_ISUPPORT(Client))
				return DISCONNECTED;
			return IRC_WriteStrClient(Client, RPL_ENDOFMOTD_MSG,
						  Client_ID(Client));
		} else {
#endif
			if (Conf_MaxNickLength != CLIENT_NICK_LEN_DEFAULT)
				Log(LOG_CRIT,
				    "Attention: this server uses a non-standard nick length, but the peer doesn't support the IRC+ extended server handshake!");
#ifdef IRCPLUS
		}
#endif

		return IRC_Num_ENDOFMOTD(Client, Req);
	}
	else if( Client_Type( Client ) == CLIENT_SERVER )
	{
		/* Neuer Server wird im Netz angekuendigt */

		/* Falsche Anzahl Parameter? */
		if( Req->argc != 4 ) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

		/* Ist ein Server mit dieser ID bereits registriert? */
		if( ! Client_CheckID( Client, Req->argv[0] )) return DISCONNECTED;

		/* Ueberfluessige Hostnamen aus Info-Text entfernen */
		ptr = strchr( Req->argv[3] + 2, '[' );
		if( ! ptr ) ptr = Req->argv[3];

		from = Client_Search( Req->prefix );
		if( ! from )
		{
			/* Hm, Server, der diesen einfuehrt, ist nicht bekannt!? */
			Log( LOG_ALERT, "Unknown ID in prefix of SERVER: \"%s\"! (on connection %d)", Req->prefix, Client_Conn( Client ));
			Conn_Close( Client_Conn( Client ), NULL, "Unknown ID in prefix of SERVER", true);
			return DISCONNECTED;
		}

		/* Neue Client-Struktur anlegen */
		c = Client_NewRemoteServer( Client, Req->argv[0], from, atoi( Req->argv[1] ), atoi( Req->argv[2] ), ptr, true);
		if( ! c )
		{
			/* Neue Client-Struktur konnte nicht angelegt werden */
			Log( LOG_ALERT, "Can't create client structure for server! (on connection %d)", Client_Conn( Client ));
			Conn_Close( Client_Conn( Client ), NULL, "Can't allocate client structure for remote server", true);
			return DISCONNECTED;
		}

		/* Log-Meldung zusammenbauen und ausgeben */
		if(( Client_Hops( c ) > 1 ) && ( Req->prefix[0] )) snprintf( str, sizeof( str ), "connected to %s, ", Client_ID( from ));
		else strcpy( str, "" );
		Log( LOG_NOTICE|LOG_snotice, "Server \"%s\" registered (via %s, %s%d hop%s).", Client_ID( c ), Client_ID( Client ), str, Client_Hops( c ), Client_Hops( c ) > 1 ? "s": "" );

		/* Andere Server informieren */
		IRC_WriteStrServersPrefix( Client, from, "SERVER %s %d %d :%s", Client_ID( c ), Client_Hops( c ) + 1, Client_MyToken( c ), Client_Info( c ));

		return CONNECTED;
	} else
		return IRC_WriteStrClient(Client, ERR_NEEDMOREPARAMS_MSG,
					  Client_ID(Client), Req->command);
} /* IRC_SERVER */


GLOBAL bool
IRC_NJOIN( CLIENT *Client, REQUEST *Req )
{
	char nick_in[COMMAND_LEN], nick_out[COMMAND_LEN], *channame, *ptr, modes[8];
	bool is_op, is_voiced;
	CHANNEL *chan;
	CLIENT *c;
	
	assert( Client != NULL );
	assert( Req != NULL );

	/* Falsche Anzahl Parameter? */
	if( Req->argc != 2 ) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

	strlcpy( nick_in, Req->argv[1], sizeof( nick_in ));
	strcpy( nick_out, "" );

	channame = Req->argv[0];
	ptr = strtok( nick_in, "," );
	while( ptr )
	{
		is_op = is_voiced = false;
		
		/* Prefixe abschneiden */
		while(( *ptr == '@' ) || ( *ptr == '+' ))
		{
			if( *ptr == '@' ) is_op = true;
			if( *ptr == '+' ) is_voiced = true;
			ptr++;
		}

		c = Client_Search( ptr );
		if( c )
		{
			Channel_Join( c, channame );
			chan = Channel_Search( channame );
			assert( chan != NULL );
			
			if( is_op ) Channel_UserModeAdd( chan, c, 'o' );
			if( is_voiced ) Channel_UserModeAdd( chan, c, 'v' );

			/* im Channel bekannt machen */
			IRC_WriteStrChannelPrefix( Client, chan, c, false, "JOIN :%s", channame );

			/* Channel-User-Modes setzen */
			strlcpy( modes, Channel_UserModes( chan, c ), sizeof( modes ));
			if( modes[0] )
			{
				/* Modes im Channel bekannt machen */
				IRC_WriteStrChannelPrefix( Client, chan, Client, false, "MODE %s +%s %s", channame, modes, Client_ID( c ));
			}

			if( nick_out[0] != '\0' ) strlcat( nick_out, ",", sizeof( nick_out ));
			if( is_op ) strlcat( nick_out, "@", sizeof( nick_out ));
			if( is_voiced ) strlcat( nick_out, "+", sizeof( nick_out ));
			strlcat( nick_out, ptr, sizeof( nick_out ));
		}
		else Log( LOG_ERR, "Got NJOIN for unknown nick \"%s\" for channel \"%s\"!", ptr, channame );
		
		/* naechsten Nick suchen */
		ptr = strtok( NULL, "," );
	}

	/* an andere Server weiterleiten */
	if( nick_out[0] != '\0' ) IRC_WriteStrServersPrefix( Client, Client_ThisServer( ), "NJOIN %s :%s", Req->argv[0], nick_out );

	return CONNECTED;
} /* IRC_NJOIN */


GLOBAL bool
IRC_SQUIT( CLIENT *Client, REQUEST *Req )
{
	CLIENT *target;
	char msg[LINE_LEN + 64];

	assert( Client != NULL );
	assert( Req != NULL );

	/* Falsche Anzahl Parameter? */
	if( Req->argc != 2 ) return IRC_WriteStrClient( Client, ERR_NEEDMOREPARAMS_MSG, Client_ID( Client ), Req->command );

	Log( LOG_DEBUG, "Got SQUIT from %s for \"%s\": \"%s\" ...", Client_ID( Client ), Req->argv[0], Req->argv[1] );

	target = Client_Search( Req->argv[0] );
	if( ! target )
	{
		/* Den Server kennen wir nicht (mehr), also nichts zu tun. */
		Log( LOG_WARNING, "Got SQUIT from %s for unknown server \"%s\"!?", Client_ID( Client ), Req->argv[0] );
		return CONNECTED;
	}

	if( Req->argv[1][0] )
	{
		if( strlen( Req->argv[1] ) > LINE_LEN ) Req->argv[1][LINE_LEN] = '\0';
		snprintf( msg, sizeof( msg ), "%s (SQUIT from %s).", Req->argv[1], Client_ID( Client ));
	}
	else snprintf( msg, sizeof( msg ), "Got SQUIT from %s.", Client_ID( Client ));

	if( Client_Conn( target ) > NONE )
	{
		/* dieser Server hat die Connection */
		if( Req->argv[1][0] ) Conn_Close( Client_Conn( target ), msg, Req->argv[1], true);
		else Conn_Close( Client_Conn( target ), msg, NULL, true);
		return DISCONNECTED;
	}
	else
	{
		/* Verbindung hielt anderer Server */
		Client_Destroy( target, msg, Req->argv[1], false );
		return CONNECTED;
	}
} /* IRC_SQUIT */


/* -eof- */
