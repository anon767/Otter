#define main main_ngircd
#include "../ngircd_comb.c"
#undef main

#include <stdio.h>
#include <string.h>
#include <iosim.h>

#define MOTDFILE "/usr/local/etc/ngircd.motd"
#define CONFFILE "/usr/local/etc/ngircd.conf"


// Defined in one of the symtest{i}.c
extern int symtest();

// Conf_UID
// Conf_GID
// Conf_ConnectRetry
// Conf_OperCanMode
// Conf_NoDNS
// Conf_ListenIPv6
// Conf_ListenIPv4
// Conf_ConnectIPv6
// Conf_ConnectIPv4
// Conf_OperServerMode
// Conf_MaxConnections
// Conf_MaxJoins
// Conf_MaxConnectionsIP
// Conf_MaxNickLength
// Conf_PingTimeout
// Conf_PongTimeout
// Conf_PredefChannelsOnly

//#define	SYM_CONF_CONNECTRETRY 
//#define	SYM_CONF_UID 
//#define	SYM_CONF_GID 
//#define	SYM_CONF_OPERCANMODE 
//#define	SYM_CONF_NODNS 
//#define	SYM_CONF_LISTENIPV4 
//#define	SYM_CONF_OPERSERVERMODE 
//#define	SYM_CONF_PREDEFCHANNELSONLY
//
//#define	SYM_CONF_MAXJOINS 
//#define	SYM_CONF_MAXCONNECTIONSIP 
//#define	SYM_CONF_MAXNICKLENGTH 
//
//#define	SYM_CONF_PINGTIMEOUT 
//#define	SYM_CONF_PONGTIMEOUT 

//#define	SYM_CONF_LISTENIPV6 	
//#define	SYM_CONF_CONNECTIPV6 	
//#define	SYM_CONF_CONNECTIPV4 	
//#define	SYM_CONF_MAXCONNECTIONS 	




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

	/* Assign values to flags
	 */
	#ifdef CConf_UID
		Conf_UID = CConf_UID;
	#endif
	#ifdef CConf_GID
		Conf_GID = CConf_GID;
	#endif
	#ifdef CConf_ConnectRetry
		Conf_ConnectRetry = CConf_ConnectRetry;
	#endif
	#ifdef CConf_OperCanMode
		Conf_OperCanMode = CConf_OperCanMode;
	#endif
	#ifdef CConf_NoDNS
		Conf_NoDNS = CConf_NoDNS;
	#endif
	#ifdef CConf_ListenIPv6
		Conf_ListenIPv6 = CConf_ListenIPv6;
	#endif
	#ifdef CConf_ListenIPv4
		Conf_ListenIPv4 = CConf_ListenIPv4;
	#endif
	#ifdef CConf_ConnectIPv6
		Conf_ConnectIPv6 = CConf_ConnectIPv6;
	#endif
	#ifdef CConf_ConnectIPv4
		Conf_ConnectIPv4 = CConf_ConnectIPv4;
	#endif
	#ifdef CConf_OperServerMode
		Conf_OperServerMode = CConf_OperServerMode;
	#endif
	#ifdef CConf_MaxConnections
		Conf_MaxConnections = CConf_MaxConnections;
	#endif
	#ifdef CConf_MaxJoins
		Conf_MaxJoins = CConf_MaxJoins;
	#endif
	#ifdef CConf_MaxConnectionsIP
		Conf_MaxConnectionsIP = CConf_MaxConnectionsIP;
	#endif
	#ifdef CConf_MaxNickLength
		Conf_MaxNickLength = CConf_MaxNickLength;
	#endif
	#ifdef CConf_PingTimeout
		Conf_PingTimeout = CConf_PingTimeout;
	#endif
	#ifdef CConf_PongTimeout
		Conf_PongTimeout = CConf_PongTimeout;
	#endif
	#ifdef CConf_PredefChannelsOnly
		Conf_PredefChannelsOnly = CConf_PredefChannelsOnly;
	#endif

	/* Make flags symbolic
	   Will overwrite any previous concrete assignments
	 */
	#ifdef SYM_CONF_UID
		__SYMBOLIC(&Conf_UID);
	#endif
	#ifdef SYM_CONF_GID
		__SYMBOLIC(&Conf_GID);
	#endif
	#ifdef SYM_CONF_CONNECTRETRY
		__SYMBOLIC(&Conf_ConnectRetry);
		__ASSUME(Conf_ConnectRetry==5 || Conf_ConnectRetry==60);
	#endif
	#ifdef SYM_CONF_OPERCANMODE
		__SYMBOLIC_BOOL(&Conf_OperCanMode);
	#endif
	#ifdef SYM_CONF_NODNS
		__SYMBOLIC_BOOL(&Conf_NoDNS);
	#endif
	#ifdef SYM_CONF_LISTENIPV6
		__SYMBOLIC_BOOL(&Conf_ListenIPv6);
	#endif
	#ifdef SYM_CONF_LISTENIPV4
		__SYMBOLIC_BOOL(&Conf_ListenIPv4);
	#endif
	#ifdef SYM_CONF_CONNECTIPV6
		__SYMBOLIC_BOOL(&Conf_ConnectIPv6);
	#endif
	#ifdef SYM_CONF_CONNECTIPV4
		__SYMBOLIC_BOOL(&Conf_ConnectIPv4);
	#endif
	#ifdef SYM_CONF_OPERSERVERMODE
		__SYMBOLIC_BOOL(&Conf_OperServerMode);
	#endif
	#ifdef SYM_CONF_MAXCONNECTIONS
		 __SYMBOLIC(&Conf_MaxConnections);
		 __ASSUME(Conf_MaxConnections==1 || Conf_MaxConnections==20 || Conf_MaxConnections==0); // 0: unlimited
	#endif
	#ifdef SYM_CONF_MAXJOINS
		 __SYMBOLIC(&Conf_MaxJoins);
		 //__ASSUME(Conf_MaxJoins>=0);
	#endif
	#ifdef SYM_CONF_MAXCONNECTIONSIP 
		 __SYMBOLIC(&Conf_MaxConnectionsIP);
		 //__ASSUME(Conf_MaxConnectionsIP>=0);
	#endif
	#ifdef SYM_CONF_MAXNICKLENGTH
		 __SYMBOLIC(&Conf_MaxNickLength);
		 //__ASSUME(Conf_MaxNickLength>=0);
	#endif
	#ifdef SYM_CONF_PINGTIMEOUT
		__SYMBOLIC(&Conf_PingTimeout);
		__ASSUME(Conf_PingTimeout==3600 || Conf_PingTimeout==120 || Conf_PingTimeout==1);
	#endif
	#ifdef SYM_CONF_PONGTIMEOUT
		__SYMBOLIC(&Conf_PongTimeout);
		__ASSUME(Conf_PongTimeout==3600 || Conf_PongTimeout==20 || Conf_PongTimeout==1);
	#endif
	#ifdef SYM_CONF_PREDEFCHANNELSONLY
		__SYMBOLIC_BOOL(&Conf_PredefChannelsOnly);
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
	Host = connect-to-host.the.net
	Bind = 10.0.0.1
	Port = 6667
	MyPassword = password
	PeerPassword = PeerSecret
	Group = 123
	Passive = no
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
	Name = TheName oh 
	Topic = a great topic
	Modes = tnk
	Key = Secret
	MaxUsers = 23
[Channel]";

	return confStr;
}

int   myargc 	 = 2;
char  myargstr[] = "ngircd\0-n\0--configtest\0          ";
char* myargv[]   = {myargstr,myargstr+7,myargstr+10};

int main(){

	symtest_Conf_Init = symtest_Conf_Init_impl;


	IOSIM_fd[0] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[0]->fd = 0;
	IOSIM_fd[0]->offset = 0;
	IOSIM_fd[0]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[0]->sym_file->contents = malloc(1);
	IOSIM_fd[0]->sym_file->stat.st_size = 1;
	IOSIM_fd[0]->sym_file->stat.st_mode = S_IFSOCK;//

	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->offsetout = 0;
	IOSIM_fd[1]->sym_fileout = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_fileout->contents = malloc(1);
	IOSIM_fd[1]->sym_fileout->stat.st_size = 1024;
	IOSIM_fd[1]->sym_fileout->stat.st_mode = S_IFSOCK;//

	stdin = IOSIM_fd[0];
	stdout = IOSIM_fd[1];
	stderr = IOSIM_fd[1];

	sym_file_t* motdFile = IOSIM_addfile(MOTDFILE,0);
	motdFile->contents = strdup("This is the content of <motdFile>\n");
	motdFile->stat.st_size = strlen(motdFile->contents);

	sym_file_t* confFile = IOSIM_addfile(CONFFILE,0);
	confFile->contents = confString();
	confFile->stat.st_size = strlen(confFile->contents);


	symtest();


	int ngircd_exit = main_ngircd(myargc,myargv);

	return 0;
}


