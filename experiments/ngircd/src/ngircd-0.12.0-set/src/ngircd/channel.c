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
 * Channel management
 */


#define __channel_c__


#include "portab.h"

static char UNUSED id[] = "$Id: channel.c,v 1.65 2008/02/05 16:31:35 fw Exp $";

#include "imp.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <strings.h>

#include "defines.h"
#include "conn-func.h"
#include "client.h"

#include "exp.h"
#include "channel.h"

#include "imp.h"
#include "irc-write.h"
#include "resolve.h"
#include "conf.h"
#include "hash.h"
#include "lists.h"
#include "log.h"
#include "messages.h"

#include "exp.h"


#define REMOVE_PART 0
#define REMOVE_QUIT 1
#define REMOVE_KICK 2

#ifndef __ORIGINAL_NGIRCD__
#include "abstractset.h"
#endif


#ifdef __ORIGINAL_NGIRCD__
static CHANNEL *My_Channels;
static CL2CHAN *My_Cl2Chan;
#else
static __SET My_Channels;
static __SET My_Cl2Chan;
#endif


static CL2CHAN *Get_Cl2Chan PARAMS(( CHANNEL *Chan, CLIENT *Client ));
static CL2CHAN *Add_Client PARAMS(( CHANNEL *Chan, CLIENT *Client ));
static bool Remove_Client PARAMS(( int Type, CHANNEL *Chan, CLIENT *Client, CLIENT *Origin, const char *Reason, bool InformServer ));
static CL2CHAN *Get_First_Cl2Chan PARAMS(( CLIENT *Client, CHANNEL *Chan ));
static CL2CHAN *Get_Next_Cl2Chan PARAMS(( CL2CHAN *Start, CLIENT *Client, CHANNEL *Chan ));
static bool Delete_Channel PARAMS(( CHANNEL *Chan ));


#ifndef __ORIGINAL_NGIRCD__
CHANNEL* Channel_Symbolic(){
	CHANNEL *c;
	c = (CHANNEL *)malloc( sizeof( CHANNEL ));
	strcpy(c->name,__SYMBOLIC_STR());
	c->hash = __SYMBOLIC(sizeof(c->hash));
	strcpy(c->modes,"");
	array_init(&c->topic);
	c->topic_time = __SYMBOLIC(sizeof(c->topic_time));
	strcpy(c->topic_who,__SYMBOLIC_STR());
	c->list_bans.first = 0 ;
	c->list_invites.first = 0 ;
	return c;
}
void* Channel_Clone(void* src_void){
    CHANNEL* src = (CHANNEL*)src_void;
    CHANNEL* tar = Channel_Symbolic();
    __CLONE(tar->name,src->name,2);
    // constraint: any channel has different name from the rest
    __ASSUME_SIMPLIFY(NOT(__STRING_EQUAL(tar->name,src->name)));
    return tar;
}
CL2CHAN* Cl2Chan_Symbolic(){
	CL2CHAN* c = malloc(sizeof(CL2CHAN));
	c->client = __SYMBOLIC(4);
	c->channel = __SYMBOLIC(4);
	strcpy(c->modes,"");
	return c;
}

void* Cl2Chan_Clone(void* src_void){
	CL2CHAN* src = (CL2CHAN*)src_void;
	CL2CHAN* tar = Cl2Chan_Symbolic();
	__CLONE(&tar->client,&src->client,sizeof(CLIENT*));
	__CLONE(&tar->channel,&src->channel,sizeof(CHANNEL*));
	__ASSUME_SIMPLIFY(OR(tar->client!=src->client,tar->channel!=src->channel));

	__SET* All_existing_Clients = Client_GetTheSet();
	__SET* All_existing_Channels = &My_Channels;
	__ASSUME_SIMPLIFY( tar->client==__SET_ABSTRACT_INTERNAL(All_existing_Clients));
	__ASSUME_SIMPLIFY( tar->channel==__SET_ABSTRACT_INTERNAL(All_existing_Channels));
	return tar;
}




#endif

GLOBAL void
Channel_Init( void )
{
#ifdef __ORIGINAL_NGIRCD__
	My_Channels = NULL;
	My_Cl2Chan = NULL;
#else
	// TODO: initialization
	__SET_INIT(&My_Channels,Channel_Symbolic(),Channel_Clone);
	__SET_INIT(&My_Cl2Chan,Cl2Chan_Symbolic(),Cl2Chan_Clone);
#endif
} /* Channel_Init */


GLOBAL struct list_head *
Channel_GetListBans(CHANNEL *c)
{
	assert(c != NULL);
	return &c->list_bans;
}


GLOBAL struct list_head *
Channel_GetListInvites(CHANNEL *c)
{
	assert(c != NULL);
	return &c->list_invites;
}


GLOBAL void
Channel_InitPredefined( void )
{
	/* Vordefinierte persistente Channels erzeugen */

	CHANNEL *chan;
	char *c;
	unsigned int i;
	
	for( i = 0; i < Conf_Channel_Count; i++ )
	{
		/* Ist ein Name konfiguriert? */
		if( ! Conf_Channel[i].name[0] ) continue;

		/* Gueltiger Channel-Name? */
		if( ! Channel_IsValidName( Conf_Channel[i].name ))
		{
			Log( LOG_ERR, "Can't create pre-defined channel: invalid name: \"%s\"!", Conf_Channel[i].name );
			array_free(&Conf_Channel[i].topic);
			continue;
		}

		/* Gibt es den Channel bereits? */
		chan = Channel_Search( Conf_Channel[i].name );
		if( chan )
		{
			Log( LOG_INFO, "Can't create pre-defined channel \"%s\": name already in use.", Conf_Channel[i].name );
			array_free(&Conf_Channel[i].topic);
			continue;
		}

		/* Create channel */
		chan = Channel_Create(Conf_Channel[i].name);
		if (chan) {
			Channel_ModeAdd(chan, 'P');

			if (array_start(&Conf_Channel[i].topic) != NULL)
				Channel_SetTopic(chan, NULL,
					 array_start(&Conf_Channel[i].topic));
			array_free(&Conf_Channel[i].topic);

			c = Conf_Channel[i].modes;
			while (*c)
				Channel_ModeAdd(chan, *c++);

			Channel_SetKey(chan, Conf_Channel[i].key);
			Channel_SetMaxUsers(chan, Conf_Channel[i].maxusers);

			Log(LOG_INFO, "Created pre-defined channel \"%s\".",
							Conf_Channel[i].name );
		}
		else Log(LOG_ERR, "Can't create pre-defined channel \"%s\"!",
							Conf_Channel[i].name );
	}
} /* Channel_InitPredefined */


GLOBAL void
Channel_Exit( void )
{
	CHANNEL *c, *c_next;
	CL2CHAN *cl2chan, *cl2chan_next;

	/* Channel-Strukturen freigeben */
#ifdef __ORIGINAL_NGIRCD__
	c = My_Channels;
	while( c )
	{
		c_next = c->next;
		array_free(&c->topic);
		free( c );
		c = c_next;
	}
#else
	// TODO: clear My_Channels 
#endif

	/* Channel-Zuordnungstabelle freigeben */
#ifdef __ORIGINAL_NGIRCD__
	cl2chan = My_Cl2Chan;
	while( c )
	{
		cl2chan_next = cl2chan->next;
		free( cl2chan );
		cl2chan = cl2chan_next;
	}
#else
	// TODO: clear My_Cl2Chan 
#endif
} /* Channel_Exit */


GLOBAL bool
Channel_Join( CLIENT *Client, char *Name )
{
	CHANNEL *chan;
	
	assert( Client != NULL );
	assert( Name != NULL );

	if( ! Channel_IsValidName( Name )) {
		IRC_WriteStrClient( Client, ERR_NOSUCHCHANNEL_MSG, Client_ID( Client ), Name );
		return false;
	}

	chan = Channel_Search( Name );
	if( chan ) {
		/* Ist der Client bereits Mitglied? */
		if( Get_Cl2Chan( chan, Client )) return false;
	}
	else
	{
		/* Gibt es noch nicht? Dann neu anlegen: */
		chan = Channel_Create( Name );
		if (!chan) return false;
	}

	/* User dem Channel hinzufuegen */
	if( ! Add_Client( chan, Client )) return false;
	else return true;
} /* Channel_Join */


/**
 * Remove client from channel.
 * This function lets a client lead a channel. First, the function checks
 * if the channel exists and the client is a member of it and sends out
 * appropriate error messages if not. The real work is done by the function
 * Remove_Client().
 */
GLOBAL bool
Channel_Part(CLIENT * Client, CLIENT * Origin, const char *Name, const char *Reason)
{
	CHANNEL *chan;

	assert(Client != NULL);
	assert(Name != NULL);
	assert(Reason != NULL);

	chan = Channel_Search(Name);
	if (!chan) {
		IRC_WriteStrClient(Client, ERR_NOSUCHCHANNEL_MSG,
				   Client_ID(Client), Name);
		return false;
	}
	if (!Get_Cl2Chan(chan, Client)) {
		IRC_WriteStrClient(Client, ERR_NOTONCHANNEL_MSG,
				   Client_ID(Client), Name);
		return false;
	}

	if (!Remove_Client(REMOVE_PART, chan, Client, Origin, Reason, true))
		return false;
	else
		return true;
} /* Channel_Part */


GLOBAL void
Channel_Kick( CLIENT *Client, CLIENT *Origin, char *Name, char *Reason )
{
	CHANNEL *chan;

	assert( Client != NULL );
	assert( Origin != NULL );
	assert( Name != NULL );
	assert( Reason != NULL );

	chan = Channel_Search( Name );
	if( ! chan )
	{
		IRC_WriteStrClient( Origin, ERR_NOSUCHCHANNEL_MSG, Client_ID( Origin ), Name );
		return;
	}

	if( ! Channel_IsMemberOf( chan, Origin ))
	{
		IRC_WriteStrClient( Origin, ERR_NOTONCHANNEL_MSG, Client_ID( Origin ), Name );
		return;
	}

	/* Is User Channel-Operator? */
	if( ! strchr( Channel_UserModes( chan, Origin ), 'o' ))
	{
		IRC_WriteStrClient( Origin, ERR_CHANOPRIVSNEEDED_MSG, Client_ID( Origin ), Name);
		return;
	}

	/* Ist the kickED User member of channel? */
	if( ! Channel_IsMemberOf( chan, Client ))
	{
		IRC_WriteStrClient( Origin, ERR_USERNOTINCHANNEL_MSG, Client_ID( Origin ), Client_ID( Client ), Name );
		return;
	}

	Remove_Client( REMOVE_KICK, chan, Client, Origin, Reason, true);
} /* Channel_Kick */

#ifndef __ORIGINAL_NGIRCD__
void Channel_Quit_Iter(void** pars,void* c){
	CLIENT *Client = pars[0];
	char *Reason = pars[1];
	Remove_Client( REMOVE_QUIT, c, Client, Client, Reason, false );
}
#endif
GLOBAL void
Channel_Quit( CLIENT *Client, char *Reason )
{
	CHANNEL *c, *next_c;

	assert( Client != NULL );
	assert( Reason != NULL );

	IRC_WriteStrRelatedPrefix( Client, Client, false, "QUIT :%s", Reason );

#ifdef __ORIGINAL_NGIRCD__
	c = My_Channels;
	while( c )
	{
		next_c = c->next;
		Remove_Client( REMOVE_QUIT, c, Client, Client, Reason, false );
		c = next_c;
	}
#else
	__SET_ITERATE(&My_Channels,Channel_Quit_Iter,__ARG(2,Client,Reason));
#endif
} /* Channel_Quit */


GLOBAL unsigned long
Channel_Count( void )
{
	CHANNEL *c;
	unsigned long count = 0;
	
#ifdef __ORIGINAL_NGIRCD__
	c = My_Channels;
	while( c )
	{
		count++;
		c = c->next;
	}
	return count;
#else
	return __SET_SIZE(&My_Channels);
#endif
} /* Channel_Count */


#ifndef __ORIGINAL_NGIRCD__
int Channel_MemberCount_Find(void** pars,void* cl2chan_void){
	CL2CHAN *cl2chan = cl2chan_void;
	CHANNEL *Chan = pars[0];
	return cl2chan->channel == Chan;
}
#endif
GLOBAL unsigned long
Channel_MemberCount( CHANNEL *Chan )
{
	CL2CHAN *cl2chan;
	unsigned long count = 0;

	assert( Chan != NULL );

#ifdef __ORIGINAL_NGIRCD__
	cl2chan = My_Cl2Chan;
	while( cl2chan )
	{
		if( cl2chan->channel == Chan ) count++;
		cl2chan = cl2chan->next;
	}
	return count;
#else
	return __SET_FIND(&cl2chan,&My_Cl2Chan,Channel_MemberCount_Find,__ARG(1,Chan)/* P(x) if x->channel == Chan */);
#endif
} /* Channel_MemberCount */


GLOBAL int
Channel_CountForUser( CLIENT *Client )
{
	/* Count number of channels a user is member of. */

	CL2CHAN *cl2chan;
	int count = 0;
	
	assert( Client != NULL );
	
#ifdef __ORIGINAL_NGIRCD__
	cl2chan = My_Cl2Chan;
	while( cl2chan )
	{
		if( cl2chan->client == Client ) count++;
		cl2chan = cl2chan->next;
	}

	return count;
#else
	// TODO
	return __SET_FIND(&cl2chan,&My_Cl2Chan,__SET_FALSE_PRED,__ARG(0)/* P(x) if x->client == Client */);
#endif
} /* Channel_CountForUser */



GLOBAL const char *
Channel_Name( const CHANNEL *Chan )
{
	assert( Chan != NULL );
	return Chan->name;
} /* Channel_Name */


GLOBAL char *
Channel_Modes( CHANNEL *Chan )
{
	assert( Chan != NULL );
	return Chan->modes;
} /* Channel_Modes */


GLOBAL char *
Channel_Key( CHANNEL *Chan )
{
	assert( Chan != NULL );
	return Chan->key;
} /* Channel_Key */


GLOBAL unsigned long
Channel_MaxUsers( CHANNEL *Chan )
{
	assert( Chan != NULL );
	return Chan->maxusers;
} /* Channel_MaxUsers */


GLOBAL CHANNEL *
Channel_First( void )
{
#ifdef __ORIGINAL_NGIRCD__
	return My_Channels;
#else
	// This function should not be called since we don't want the set to be iterated manuallly
	assert(0);
	return 0;
#endif
} /* Channel_First */


GLOBAL CHANNEL *
Channel_Next( CHANNEL *Chan )
{
#ifdef __ORIGINAL_NGIRCD__
	assert( Chan != NULL );
	return Chan->next;
#else
	// This function should not be called since we don't want the set to be iterated manuallly
	assert(0);
	return 0;
#endif
} /* Channel_Next */


#ifndef __ORIGINAL_NGIRCD__
int Channel_Search_Find(void** pars,void* c_void){
	CHANNEL *c = c_void;
	char *Name = pars[0];
	//return ( strcasecmp( Name, c->name ) == 0 ) ;
	// procedures in any pred cannot contain if stmts
	return ( __STRING_EQUAL( Name, c->name )  ) ;
}
#endif
GLOBAL CHANNEL *
Channel_Search( const char *Name )
{
	/* Channel-Struktur suchen */
	
	CHANNEL *c;
	UINT32 search_hash;

	assert( Name != NULL );

#ifdef __ORIGINAL_NGIRCD__
	search_hash = Hash( Name );
	c = My_Channels;
	while( c )
	{
		if( search_hash == c->hash )
		{
			/* lt. Hash-Wert: Treffer! */
			if( strcasecmp( Name, c->name ) == 0 ) return c;
		}
		c = c->next;
	}
#else
	if(__SET_FIND(&c,&My_Channels,Channel_Search_Find,__ARG(1,Name)/* P(x) if strcasecmp(Name,x->name)==0 */)>0)
		return c;
#endif
	return NULL;
} /* Channel_Search */


GLOBAL CL2CHAN *
Channel_FirstMember( CHANNEL *Chan )
{
#ifdef __ORIGINAL_NGIRCD__
	assert( Chan != NULL );
	return Get_First_Cl2Chan( NULL, Chan );
#else
	assert(0);
	return 0;
#endif
} /* Channel_FirstMember */


GLOBAL CL2CHAN *
Channel_NextMember( CHANNEL *Chan, CL2CHAN *Cl2Chan )
{
#ifdef __ORIGINAL_NGIRCD__
	assert( Chan != NULL );
	assert( Cl2Chan != NULL );
	return Get_Next_Cl2Chan( Cl2Chan->next, NULL, Chan );
#else
	assert(0);
	return 0;
#endif
} /* Channel_NextMember */


GLOBAL CL2CHAN *
Channel_FirstChannelOf( CLIENT *Client )
{
#ifdef __ORIGINAL_NGIRCD__
	assert( Client != NULL );
	return Get_First_Cl2Chan( Client, NULL );
#else
	assert(0);
	return 0;
#endif
} /* Channel_FirstChannelOf */


GLOBAL CL2CHAN *
Channel_NextChannelOf( CLIENT *Client, CL2CHAN *Cl2Chan )
{
#ifdef __ORIGINAL_NGIRCD__
	assert( Client != NULL );
	assert( Cl2Chan != NULL );
	return Get_Next_Cl2Chan( Cl2Chan->next, Client, NULL );
#else
	assert(0);
	return 0;
#endif
} /* Channel_NextChannelOf */


GLOBAL CLIENT *
Channel_GetClient( CL2CHAN *Cl2Chan )
{
	assert( Cl2Chan != NULL );
	return Cl2Chan->client;
} /* Channel_GetClient */


GLOBAL CHANNEL *
Channel_GetChannel( CL2CHAN *Cl2Chan )
{
	assert( Cl2Chan != NULL );
	return Cl2Chan->channel;
} /* Channel_GetChannel */


GLOBAL bool
Channel_IsValidName( const char *Name )
{
	assert( Name != NULL );

	if(( Name[0] != '#' ) || ( strlen( Name ) >= CHANNEL_NAME_LEN )) return false;

	return Name[strcspn(Name, " ,:\007")] == 0;
} /* Channel_IsValidName */


GLOBAL bool
Channel_ModeAdd( CHANNEL *Chan, char Mode )
{
	/* set Mode.
	 * If the channel already had this mode, return false.
	 * If the channel mode was newly set return true.
	 */

	char x[2];

	assert( Chan != NULL );

	x[0] = Mode; x[1] = '\0';
	if( ! strchr( Chan->modes, x[0] ))
	{
		/* Channel does not have this mode yet, set it */
		strlcat( Chan->modes, x, sizeof( Chan->modes ));
		return true;
	}
	else return false;
} /* Channel_ModeAdd */


GLOBAL bool
Channel_ModeDel( CHANNEL *Chan, char Mode )
{
	/* Delete mode.
	 * if the mode was removed return true.
	 * if the channel did not have the mode, return false.
	*/
	char *p;

	assert( Chan != NULL );

	p = strchr( Chan->modes, Mode );
	if( ! p ) return false;

	/* Channel has mode -> delete */
	while( *p )
	{
		*p = *(p + 1);
		p++;
	}
	return true;
} /* Channel_ModeDel */


GLOBAL bool
Channel_UserModeAdd( CHANNEL *Chan, CLIENT *Client, char Mode )
{
	/* Set Channel-User-Mode.
	 * if mode was newly set, return true.
	 * if the User already had this channel-mode, return false.
	 */

	CL2CHAN *cl2chan;
	char x[2];

	assert( Chan != NULL );
	assert( Client != NULL );

	cl2chan = Get_Cl2Chan( Chan, Client );
	assert( cl2chan != NULL );

	x[0] = Mode; x[1] = '\0';
	if( ! strchr( cl2chan->modes, x[0] ))
	{
		/* mode not set, -> set it */
		strlcat( cl2chan->modes, x, sizeof( cl2chan->modes ));
		return true;
	}
	else return false;
} /* Channel_UserModeAdd */


GLOBAL bool
Channel_UserModeDel( CHANNEL *Chan, CLIENT *Client, char Mode )
{
	/* Delete Channel-User-Mode.
	 * If Mode was removed, return true.
	 * If User did not have the Channel-Mode, return false.
	 */

	CL2CHAN *cl2chan;
	char *p;

	assert( Chan != NULL );
	assert( Client != NULL );

	cl2chan = Get_Cl2Chan( Chan, Client );
	assert( cl2chan != NULL );

	p = strchr( cl2chan->modes, Mode );
	if( ! p ) return false;

	/* Client has Mode -> delete */
	while( *p )
	{
		*p = *(p + 1);
		p++;
	}
	return true;
} /* Channel_UserModeDel */


GLOBAL char *
Channel_UserModes( CHANNEL *Chan, CLIENT *Client )
{
	/* return Users' Channel-Modes */

	CL2CHAN *cl2chan;

	assert( Chan != NULL );
	assert( Client != NULL );

	cl2chan = Get_Cl2Chan( Chan, Client );
	assert( cl2chan != NULL );

	return cl2chan->modes;
} /* Channel_UserModes */


GLOBAL bool
Channel_IsMemberOf( CHANNEL *Chan, CLIENT *Client )
{
	/* Test if Client is on Channel Chan */

	assert( Chan != NULL );
	assert( Client != NULL );
	return Get_Cl2Chan(Chan, Client) != NULL;
} /* Channel_IsMemberOf */


GLOBAL char *
Channel_Topic( CHANNEL *Chan )
{
	char *ret;
	assert( Chan != NULL );
	ret = array_start(&Chan->topic);
	return ret ? ret : "";
} /* Channel_Topic */

	
#ifndef STRICT_RFC

GLOBAL unsigned int
Channel_TopicTime(CHANNEL *Chan)
{
	assert(Chan != NULL);
	return (unsigned int) Chan->topic_time;
} /* Channel_TopicTime */


GLOBAL char *
Channel_TopicWho(CHANNEL *Chan)
{
	assert(Chan != NULL);
	return Chan->topic_who;
} /* Channel_TopicWho */

#endif


GLOBAL void
Channel_SetTopic(CHANNEL *Chan, CLIENT *Client, char *Topic)
{
	size_t len;
	assert( Chan != NULL );
	assert( Topic != NULL );

	len = strlen(Topic);
	if (len < array_bytes(&Chan->topic))
		array_free(&Chan->topic);

	if (len >= COMMAND_LEN || !array_copyb(&Chan->topic, Topic, len+1))
		Log(LOG_WARNING, "could not set new Topic \"%s\" on %s: %s",
					Topic, Chan->name, strerror(errno));
#ifndef STRICT_RFC
	Chan->topic_time = time(NULL);
	if (Client != NULL && Client_Type(Client) != CLIENT_SERVER)
		strlcpy(Chan->topic_who, Client_ID(Client),
			sizeof Chan->topic_who);
	else
		strlcpy(Chan->topic_who, DEFAULT_TOPIC_ID,
			sizeof Chan->topic_who);
#else
	(void) Client;
#endif
} /* Channel_SetTopic */


GLOBAL void
Channel_SetModes( CHANNEL *Chan, char *Modes )
{
	assert( Chan != NULL );
	assert( Modes != NULL );

	strlcpy( Chan->modes, Modes, sizeof( Chan->modes ));
} /* Channel_SetModes */


GLOBAL void
Channel_SetKey( CHANNEL *Chan, char *Key )
{
	assert( Chan != NULL );
	assert( Key != NULL );

	strlcpy( Chan->key, Key, sizeof( Chan->key ));
	LogDebug("Channel %s: Key is now \"%s\".", Chan->name, Chan->key );
} /* Channel_SetKey */


GLOBAL void
Channel_SetMaxUsers(CHANNEL *Chan, unsigned long Count)
{
	assert( Chan != NULL );

	Chan->maxusers = Count;
	LogDebug("Channel %s: Member limit is now %lu.", Chan->name, Chan->maxusers );
} /* Channel_SetMaxUsers */


static bool
Can_Send_To_Channel(CHANNEL *Chan, CLIENT *From)
{
	bool is_member, has_voice, is_op;

	is_member = has_voice = is_op = false;

	if (Channel_IsMemberOf(Chan, From)) {
		is_member = true;
		if (strchr(Channel_UserModes(Chan, From), 'v'))
			has_voice = true;
		if (strchr(Channel_UserModes(Chan, From), 'o'))
			is_op = true;
	}

	/*
	 * Is the client allowed to write to channel?
	 *
	 * If channel mode n set: non-members cannot send to channel.
	 * If channel mode m set: need voice.
	 */
	if (strchr(Channel_Modes(Chan), 'n') && !is_member)
		return false;

	if (is_op || has_voice)
		return true;

	if (strchr(Channel_Modes(Chan), 'm'))
		return false;

	return !Lists_Check(&Chan->list_bans, From);
}


GLOBAL bool
Channel_Write(CHANNEL *Chan, CLIENT *From, CLIENT *Client, const char *Text)
{
	if (!Can_Send_To_Channel(Chan, From))
		return IRC_WriteStrClient(From, ERR_CANNOTSENDTOCHAN_MSG, Client_ID(From), Channel_Name(Chan));

	if (Client_Conn(From) > NONE)
		Conn_UpdateIdle(Client_Conn(From));

	return IRC_WriteStrChannelPrefix(Client, Chan, From, true,
			"PRIVMSG %s :%s", Channel_Name(Chan), Text);
}


GLOBAL bool
Channel_Notice(CHANNEL *Chan, CLIENT *From, CLIENT *Client, const char *Text)
{
	if (!Can_Send_To_Channel(Chan, From))
		return true; /* no error, see RFC 2812 */

	if (Client_Conn(From) > NONE)
		Conn_UpdateIdle(Client_Conn(From));

	return IRC_WriteStrChannelPrefix(Client, Chan, From, true,
			"NOTICE %s :%s", Channel_Name(Chan), Text);
}


GLOBAL CHANNEL *
Channel_Create( char *Name )
{
	/* Create new CHANNEL structure and add it to linked list */
	CHANNEL *c;

	assert( Name != NULL );

	c = (CHANNEL *)malloc( sizeof( CHANNEL ));
	if( ! c )
	{
		Log( LOG_EMERG, "Can't allocate memory! [New_Chan]" );
		return NULL;
	}
	memset( c, 0, sizeof( CHANNEL ));
	strlcpy( c->name, Name, sizeof( c->name ));
#ifdef __ORIGINAL_NGIRCD__
	c->hash = Hash( c->name );
	c->next = My_Channels;
	My_Channels = c;
#else
	__SET_ADD(c,&My_Channels);
#endif
	LogDebug("Created new channel structure for \"%s\".", Name);
	return c;
} /* Channel_Create */


#ifndef __ORIGINAL_NGIRCD__
int Get_Cl2Chan_Find(void** pars,void* cl2chan_void){
	CL2CHAN *cl2chan = cl2chan_void;
	CHANNEL *Chan = pars[0];
	CLIENT *Client = pars[1];
	return AND(( cl2chan->channel == Chan ) , ( cl2chan->client == Client )) ;
}
#endif
static CL2CHAN *
Get_Cl2Chan( CHANNEL *Chan, CLIENT *Client )
{
	CL2CHAN *cl2chan;

	assert( Chan != NULL );
	assert( Client != NULL );

#ifdef __ORIGINAL_NGIRCD__
	cl2chan = My_Cl2Chan;
	while( cl2chan )
	{
		if(( cl2chan->channel == Chan ) && ( cl2chan->client == Client )) return cl2chan;
		cl2chan = cl2chan->next;
	}
#else
	if(__SET_FIND(&cl2chan,&My_Cl2Chan,Get_Cl2Chan_Find,__ARG(2,Chan,Client)/* P(x) if x->channel==Chan && x->client==Client */) >0)
		return cl2chan;
#endif
	return NULL;
} /* Get_Cl2Chan */


static CL2CHAN *
Add_Client( CHANNEL *Chan, CLIENT *Client )
{
	CL2CHAN *cl2chan;

	assert( Chan != NULL );
	assert( Client != NULL );

	/* neue CL2CHAN-Struktur anlegen */
	cl2chan = (CL2CHAN *)malloc( sizeof( CL2CHAN ));
	if( ! cl2chan )
	{
		Log( LOG_EMERG, "Can't allocate memory! [Add_Client]" );
		return NULL;
	}
	cl2chan->channel = Chan;
	cl2chan->client = Client;
	strcpy( cl2chan->modes, "" );

#ifdef __ORIGINAL_NGIRCD__
	/* Verketten */
	cl2chan->next = My_Cl2Chan;
	My_Cl2Chan = cl2chan;
#else
	__SET_ADD(cl2chan,&My_Cl2Chan);
#endif

	Log( LOG_DEBUG, "User \"%s\" joined channel \"%s\".", Client_Mask( Client ), Chan->name );

	return cl2chan;
} /* Add_Client */


static bool
Remove_Client( int Type, CHANNEL *Chan, CLIENT *Client, CLIENT *Origin, const char *Reason, bool InformServer )
{
	CL2CHAN *cl2chan, *last_cl2chan;
	CHANNEL *c;
	
	assert( Chan != NULL );
	assert( Client != NULL );
	assert( Origin != NULL );
	assert( Reason != NULL );

#ifdef __ORIGINAL_NGIRCD__
	last_cl2chan = NULL;
	cl2chan = My_Cl2Chan;
	while( cl2chan )
	{
		if(( cl2chan->channel == Chan ) && ( cl2chan->client == Client )) break;
		last_cl2chan = cl2chan;
		cl2chan = cl2chan->next;
	}
	if( ! cl2chan ) return false;
#else
	// TODO
	if(__SET_FIND(&cl2chan,&My_Cl2Chan,__SET_FALSE_PRED,__ARG(0)/* P(x) if x->channel==Chan&&x->client==Client*/)==0)
		return false;
#endif

	c = cl2chan->channel;
	assert( c != NULL );

#ifdef __ORIGINAL_NGIRCD__
	/* Aus Verkettung loesen und freigeben */
	if( last_cl2chan ) last_cl2chan->next = cl2chan->next;
	else My_Cl2Chan = cl2chan->next;
#else
	// TODO
	__SET_REMOVE(&cl2chan,&My_Cl2Chan);
#endif
	free( cl2chan );

	switch( Type )
	{
		case REMOVE_QUIT:
			/* QUIT: other servers have already been notified, see Client_Destroy();
			 * so only inform other clients in same channel. */
			assert( InformServer == false );
			LogDebug("User \"%s\" left channel \"%s\" (%s).",
					Client_Mask( Client ), c->name, Reason );
			break;
		case REMOVE_KICK:
			/* User was KICKed: inform other servers and all users in channel */
			if( InformServer )
				IRC_WriteStrServersPrefix( Client_NextHop( Origin ),
					Origin, "KICK %s %s :%s", c->name, Client_ID( Client ), Reason);
			IRC_WriteStrChannelPrefix(Client, c, Origin, false, "KICK %s %s :%s",
							c->name, Client_ID( Client ), Reason );
			if ((Client_Conn(Client) > NONE) &&
					(Client_Type(Client) == CLIENT_USER))
			{
				IRC_WriteStrClientPrefix(Client, Origin, "KICK %s %s :%s",
								c->name, Client_ID( Client ), Reason);
			}
			LogDebug("User \"%s\" has been kicked of \"%s\" by \"%s\": %s.",
				Client_Mask( Client ), c->name, Client_ID(Origin), Reason);
			break;
		default: /* PART */
			if (InformServer)
				IRC_WriteStrServersPrefix(Origin, Client, "PART %s :%s", c->name, Reason);

			IRC_WriteStrChannelPrefix(Origin, c, Client, false, "PART %s :%s",
									c->name, Reason);

			if ((Client_Conn(Origin) > NONE) &&
					(Client_Type(Origin) == CLIENT_USER))
			{
				IRC_WriteStrClientPrefix( Origin, Client, "PART %s :%s", c->name, Reason);
				LogDebug("User \"%s\" left channel \"%s\" (%s).",
					Client_Mask(Client), c->name, Reason);
			}
	}

	/* Wenn Channel nun leer und nicht pre-defined: loeschen */
	if( ! strchr( Channel_Modes( Chan ), 'P' ))
	{
		if( ! Get_First_Cl2Chan( NULL, Chan )) Delete_Channel( Chan );
	}

	return true;
} /* Remove_Client */


GLOBAL bool
Channel_AddBan(CHANNEL *c, const char *mask )
{
	struct list_head *h = Channel_GetListBans(c);
	return Lists_Add(h, mask, false);
}


GLOBAL bool
Channel_AddInvite(CHANNEL *c, const char *mask, bool onlyonce)
{
	struct list_head *h = Channel_GetListInvites(c);
	return Lists_Add(h, mask, onlyonce);
}


static bool
ShowInvitesBans(struct list_head *head, CLIENT *Client, CHANNEL *Channel, bool invite)
{
	struct list_elem *e;
	char *msg = invite ? RPL_INVITELIST_MSG : RPL_BANLIST_MSG;
	char *msg_end;

	assert( Client != NULL );
	assert( Channel != NULL );

	e = Lists_GetFirst(head);
	while (e) {
		if( ! IRC_WriteStrClient( Client, msg, Client_ID( Client ),
				Channel_Name( Channel ), Lists_GetMask(e) )) return DISCONNECTED;
		e = Lists_GetNext(e);
	}

	msg_end = invite ? RPL_ENDOFINVITELIST_MSG : RPL_ENDOFBANLIST_MSG;
	return IRC_WriteStrClient( Client, msg_end, Client_ID( Client ), Channel_Name( Channel ));
}


GLOBAL bool
Channel_ShowBans( CLIENT *Client, CHANNEL *Channel )
{
	struct list_head *h;

	assert( Channel != NULL );

	h = Channel_GetListBans(Channel);
	return ShowInvitesBans(h, Client, Channel, false);
}


GLOBAL bool
Channel_ShowInvites( CLIENT *Client, CHANNEL *Channel )
{
	struct list_head *h;

	assert( Channel != NULL );

	h = Channel_GetListInvites(Channel);
	return ShowInvitesBans(h, Client, Channel, true);
}


static CL2CHAN *
Get_First_Cl2Chan( CLIENT *Client, CHANNEL *Chan )
{
#ifdef __ORIGINAL_NGIRCD__
	return Get_Next_Cl2Chan( My_Cl2Chan, Client, Chan );
#else
	assert(0);
	return 0;
#endif
} /* Get_First_Cl2Chan */


static CL2CHAN *
Get_Next_Cl2Chan( CL2CHAN *Start, CLIENT *Client, CHANNEL *Channel )
{
	CL2CHAN *cl2chan;

	assert( Client != NULL || Channel != NULL );
	
#ifdef __ORIGINAL_NGIRCD__
	cl2chan = Start;
	while( cl2chan )
	{
		if(( Client ) && ( cl2chan->client == Client )) return cl2chan;
		if(( Channel ) && ( cl2chan->channel == Channel )) return cl2chan;
		cl2chan = cl2chan->next;
	}
	return NULL;
#else
	// should not be called
	assert(0);
	return 0;
#endif
} /* Get_Next_Cl2Chan */


static bool
Delete_Channel( CHANNEL *Chan )
{
	/* Channel-Struktur loeschen */

	CHANNEL *chan, *last_chan;

#ifdef __ORIGINAL_NGIRCD__
	last_chan = NULL;
	chan = My_Channels;
	while( chan )
	{
		if( chan == Chan ) break;
		last_chan = chan;
		chan = chan->next;
	}
	if( ! chan ) return false;
#else
	// TODO: need that?
	if(__SET_FIND(&chan,&My_Channels,__SET_FALSE_PRED,__ARG(0)/* P(x) if chan==Chan */)==0)
		return false;
#endif

	Log( LOG_DEBUG, "Freed channel structure for \"%s\".", Chan->name );

	/* Invite- und Ban-Lists aufraeumen */
	Lists_Free( &chan->list_bans );
	Lists_Free( &chan->list_invites );

#ifdef __ORIGINAL_NGIRCD__
	/* Neu verketten und freigeben */
	if( last_chan ) last_chan->next = chan->next;
	else My_Channels = chan->next;
#else
	// TODO
	__SET_REMOVE(&chan,&My_Channels);
#endif
	free( chan );

	return true;
} /* Delete_Channel */


/* -eof- */
