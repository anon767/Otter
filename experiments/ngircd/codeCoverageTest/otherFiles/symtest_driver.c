#define main main_ngircd
#include "../ngircd_comb.c"
#undef main

#include <stdio.h>
#include <string.h>
#include <iosim.h>

#define MOTDFILE "/usr/local/etc/ngircd.motd"
#define CONFFILE "/usr/local/etc/ngircd.conf"

int symtest_configtest = 0;

// Defined in one of the symtest{i}.c
extern int symtest();

// define if do not make the flag symbolic
//#define	CONF_CONNECTRETRY 
//#define	CONF_UID 
//#define	CONF_GID 
//#define	CONF_OPERCANMODE 
//#define	CONF_NODNS 
//#define	CONF_LISTENIPV6 
//#define	CONF_LISTENIPV4 
//#define	CONF_OPERSERVERMODE 
//#define	CONF_CONNECTIPV6 
//#define	CONF_CONNECTIPV4 
//
//#define	CONF_MAXJOINS 
//#define	CONF_MAXCONNECTIONSIP 
//#define	CONF_MAXNICKLENGTH 
//
//#define	CONF_PINGTIMEOUT 
//#define	CONF_PONGTIMEOUT 
#define	CONF_MAXCONNECTIONS 

#define __SYMBOLIC_BOOL(X)  \
{ \
	__SYMBOLIC(X); \
	__ASSUME((*(X))==0 || (*(X))==1); \
}

void symtest_Conf_Init_impl(){

	// this function is stepped in only once.
	static int called = 0;
	if(called==0)
		called = 1;
	else
		return;

	#ifndef CONF_UID
		__SYMBOLIC_BOOL(&Conf_UID);
	#endif
		 
	#ifndef CONF_GID
		__SYMBOLIC_BOOL(&Conf_GID);
	#endif
		 
	#ifndef CONF_CONNECTRETRY
		__SYMBOLIC_BOOL(&Conf_ConnectRetry);
	#endif
		 
	#ifndef CONF_OPERCANMODE
		__SYMBOLIC_BOOL(&Conf_OperCanMode);
	#endif
		 
	#ifndef CONF_NODNS
		__SYMBOLIC_BOOL(&Conf_NoDNS);
	#endif
		 
	#ifndef CONF_LISTENIPV6
		__SYMBOLIC_BOOL(&Conf_ListenIPv6);
	#endif
		 
	#ifndef CONF_LISTENIPV4
		__SYMBOLIC_BOOL(&Conf_ListenIPv4);
	#endif
		 
	#ifndef CONF_CONNECTIPV6
		__SYMBOLIC_BOOL(&Conf_ConnectIPv6);
	#endif
		 
	#ifndef CONF_CONNECTIPV4
		__SYMBOLIC_BOOL(&Conf_ConnectIPv4);
	#endif
		 
	#ifndef CONF_OPERSERVERMODE
		__SYMBOLIC_BOOL(&Conf_OperServerMode);
	#endif
		 
	#ifndef CONF_MAXCONNECTIONS
		 __SYMBOLIC(&Conf_MaxConnections);
		 __ASSUME(Conf_MaxConnections==1 || Conf_MaxConnections==20 || Conf_MaxConnections==0); // 0: unlimited
	#endif
		 
	#ifndef CONF_MAXJOINS
		 __SYMBOLIC(&Conf_MaxJoins);
		 __ASSUME(Conf_MaxJoins>=0);
	#endif
		 
	#ifndef CONF_MAXCONNECTIONSIP 
		 __SYMBOLIC(&Conf_MaxConnectionsIP);
		 __ASSUME(Conf_MaxConnectionsIP>=0);
	#endif
		 
	#ifndef CONF_MAXNICKLENGTH
		 __SYMBOLIC(&Conf_MaxNickLength);
		 __ASSUME(Conf_MaxNickLength>=0);
	#endif
		
	#ifndef CONF_PINGTIMEOUT
		__SYMBOLIC(&Conf_PingTimeout);
		__ASSUME(Conf_PingTimeout==3600 || Conf_PingTimeout==120 || Conf_PingTimeout==1);
	#endif
	
	#ifndef CONF_PONGTIMEOUT
		__SYMBOLIC(&Conf_PongTimeout);
		__ASSUME(Conf_PongTimeout==3600 || Conf_PongTimeout==20 || Conf_PongTimeout==1);
	#endif
		 
}

// Default configuration being read into ngircd
// It will be overwritten by the symbolic configuration
char* confString(){
	static char confStr[] = "[Global]
	Name = irc.the.net
	Info = Server Info Text
	;Password = abc
	;AdminInfo1 = Description
	;AdminInfo2 = Location
	;AdminEMail = admin@irc.server
	Ports = 6667
	;Listen = 1.2.3.4
	MotdFile = /usr/local/etc/ngircd.motd
	;MotdPhrase = \"Hello world!\"
	;ServerUID = 65534
	;ServerGID = 65534
	;ChrootDir = /var/empty
	;PidFile = /var/run/ngircd/ngircd.pid
	PingTimeout = 1200
	PongTimeout = 200
	;ConnectRetry = 60
	;OperCanUseMode = no
	;OperServerMode = no
	;PredefChannelsOnly = no
	;NoDNS = no
	;ListenIPv6 = yes
	;ListenIPv4 = yes
	;ConnectIPv6 = yes
	;ConnectIPv4 = yes
	MaxConnections = 0
	MaxConnectionsIP = 0
	;MaxJoins = 10
	MaxNickLength = 9
[Operator]
	Name = TheOper
	Password = ThePwd
	Mask = *
[Operator]
[Server]
	Name = irc2.the.net
	;Host = connect-to-host.the.net
	;Bind = 10.0.0.1
	;Port = 6667
	MyPassword = password
	;PeerPassword = PeerSecret
	;Group = 123
	;Passive = no
[Server]
[Channel]
	Name = #TheName
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]
	Name = #TheName
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]
	Name = #TheNameooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]";

	return confStr;
}

int main(){

	symtest_Conf_Init = symtest_Conf_Init_impl;

	int argc = 2;
	char  argstr[] = "ngircd\0-n\0--configtest";
	char* argv[]   = {argstr,argstr+7,argstr+10};
	if(symtest_configtest) argc = 3;

	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->offsetout = 0;
	IOSIM_fd[1]->sym_fileout = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_fileout->contents = malloc(1);
	IOSIM_fd[1]->sym_fileout->stat.st_size = 1024;
	IOSIM_fd[1]->sym_fileout->stat.st_mode = S_IFSOCK;//

	stdout = IOSIM_fd[1];
	stderr = IOSIM_fd[1];

	sym_file_t* motdFile = IOSIM_addfile(MOTDFILE,0);
	motdFile->contents = strdup("This is the content of <motdFile>\n");
	motdFile->stat.st_size = strlen(motdFile->contents);

	sym_file_t* confFile = IOSIM_addfile(CONFFILE,0);
	confFile->contents = confString();
	confFile->stat.st_size = strlen(confFile->contents);


	symtest();


	int ngircd_exit = main_ngircd(argc,argv);

	return 0;
}


