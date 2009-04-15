#define main main_ngircd
#include "../ngircd_comb.c"
#undef main

#include <stdio.h>
#include <string.h>
#include <iosim.h>


#define MOTDFILE "/MotdFile.txt"

void symtest_Conf_Init_impl(){
	 /* Name ("Nick") of the servers */
	 // char Conf_ServerName[CLIENT_ID_LEN];
	 strcpy(Conf_ServerName,"@Conf_ServerName");
	 
	 /* Server info text */
	 // char Conf_ServerInfo[CLIENT_INFO_LEN];
	 strcpy(Conf_ServerInfo,"@Conf_ServerInfo");
	 
	 /* // server passwort */
	 // char Conf_ServerPwd[CLIENT_PASS_LEN];
	 strcpy(Conf_ServerPwd,"");
	 //strcpy(Conf_ServerPwd,"@Conf_ServerPwd");
	 
	 /* Administrative information */
	 // char Conf_ServerAdmin1[CLIENT_INFO_LEN];
	 // char Conf_ServerAdmin2[CLIENT_INFO_LEN];
	 // char Conf_ServerAdminMail[CLIENT_INFO_LEN];
	 strcpy(Conf_ServerAdmin1,"@Conf_ServerAdmin1");
	 strcpy(Conf_ServerAdmin2,"@Conf_ServerAdmin2");
	 strcpy(Conf_ServerAdminMail,"@Conf_ServerAdminMail");
	 
	 /* File with MOTD text */
	 // char Conf_MotdFile[FNAME_LEN];
	 strcpy(Conf_MotdFile,MOTDFILE);
	 
	 /* Phrase with MOTD text */
	 // char Conf_MotdPhrase[LINE_LEN];
	 
	 /* Ports the server should listen on */
	 // array Conf_ListenPorts;
	 ports_parse(&Conf_ListenPorts,0,strdup("6667"));
	 
	 /* Address to which the socket should be bound or empty (=all) */
	 // char Conf_ListenAddress[16];
	 
	 /* User and group ID the server should run with */
	 // uid_t Conf_UID;
	 // gid_t Conf_GID;

	 
#ifdef CONF_UID
	Conf_UID = CONF_UID;
#else
	__SYMBOLIC(&Conf_UID);
#endif

	 
#ifdef CONF_GID
	Conf_GID = CONF_GID;
#else
	__SYMBOLIC(&Conf_GID);
#endif
	 
	 /* A directory to chroot() in */
	 // char Conf_Chroot[FNAME_LEN];
	 
	 /* File with PID of daemon */
	 // char Conf_PidFile[FNAME_LEN];
	 
	 /* Timeouts for PING and PONG */

	 
#ifdef CONF_PINGTIMEOUT
	Conf_PingTimeout = CONF_PINGTIMEOUT;
#else
	__SYMBOLIC(&Conf_PingTimeout);
#endif

	 
#ifdef CONF_PONGTIMEOUT
	Conf_PongTimeout = CONF_PONGTIMEOUT;
#else
	__SYMBOLIC(&Conf_PongTimeout);
#endif
	 
	 /* Seconds between connect attempts to other servers */
	 // int Conf_ConnectRetry;
	 
#ifdef CONF_CONNECTRETRY
	Conf_ConnectRetry = CONF_CONNECTRETRY;
#else
	__SYMBOLIC(&Conf_ConnectRetry);
#endif
	 
	 /* Operators */
	 // CONF_OPER Conf_Oper[MAX_OPERATORS];
	 // unsigned int Conf_Oper_Count;
	 
	 /* Servers */
	 // CONF_SERVER Conf_Server[MAX_SERVERS];
	 
	 /* Pre-defined channels */
	 // CONF_CHANNEL Conf_Channel[MAX_DEFCHANNELS];
	 // unsigned int Conf_Channel_Count;
	 /* Pre-defined channels only */
	 // bool Conf_PredefChannelsOnly;
	 
	 /* Are IRC operators allowed to always use MODE? */
	 // bool Conf_OperCanMode;
	 
#ifdef CONF_OPERCANMODE
	Conf_OperCanMode = CONF_OPERCANMODE;
#else
	__SYMBOLIC(&Conf_OperCanMode);
#endif
	 
	 /* Disable all DNS functions? */
	 // bool Conf_NoDNS;
	 
#ifdef CONF_NODNS
	Conf_NoDNS = CONF_NODNS;
#else
	__SYMBOLIC(&Conf_NoDNS);
#endif
	 
	 /* listen for incoming ipv6 connections if OS supports it (default: yes)? */
	 // bool Conf_ListenIPv6;
	 
#ifdef CONF_LISTENIPV6
	Conf_ListenIPv6 = CONF_LISTENIPV6;
#else
	__SYMBOLIC(&Conf_ListenIPv6);
#endif
	 
	 /* listen for incoming ipv4 connections if OS supports it (default: yes)? */
	 // bool Conf_ListenIPv4;
	 
#ifdef CONF_LISTENIPV4
	Conf_ListenIPv4 = CONF_LISTENIPV4;
#else
	__SYMBOLIC(&Conf_ListenIPv4);
#endif
	 
	 /*
	  * try to connect to remote systems using the ipv6 protocol,
	  * if they have an ipv6 address? (default yes)
	  */
	 // bool Conf_ConnectIPv6;
	 
#ifdef CONF_CONNECTIPV6
	Conf_ConnectIPv6 = CONF_CONNECTIPV6;
#else
	__SYMBOLIC(&Conf_ConnectIPv6);
#endif
	 
	 /* same as above, but for ipv4 hosts, default: yes  */
	 // bool Conf_ConnectIPv4;
	 
#ifdef CONF_CONNECTIPV4
	Conf_ConnectIPv4 = CONF_CONNECTIPV4;
#else
	__SYMBOLIC(&Conf_ConnectIPv4);
#endif
	 
	 /* If an IRC op gives chanop privileges without being a chanop,
	  * ircd2 will ignore the command. This enables a workaround:
	  * It masks the command as coming from the server */
	 // bool Conf_OperServerMode;
	 
#ifdef CONF_OPERSERVERMODE
	Conf_OperServerMode = CONF_OPERSERVERMODE;
#else
	__SYMBOLIC(&Conf_OperServerMode);
#endif
	 
	 /* Maximum number of connections to this server */
	 // long Conf_MaxConnections;
//	 __SYMBOLIC(&Conf_MaxConnections);
//	 __ASSUME(Conf_MaxConnections>=0);
	 
	 /* Maximum number of channels a user can join */
	 // int Conf_MaxJoins;
#ifdef CONF_MAXJOINS
	Conf_MaxJoins = CONF_MAXJOINS;
#else
	 __SYMBOLIC(&Conf_MaxJoins);
	 __ASSUME(Conf_MaxJoins>=1);
	 __ASSUME(Conf_MaxJoins<=9);
#endif
	 
	 /* Maximum number of connections per IP address */
	 // int Conf_MaxConnectionsIP;
#ifdef CONF_MAXCONNECTIONSIP 
	 Conf_MaxConnectionsIP = CONF_MAXCONNECTIONSIP;
#else
	 __SYMBOLIC(&Conf_MaxConnectionsIP);
	 __ASSUME(Conf_MaxConnectionsIP>=1);
	 __ASSUME(Conf_MaxConnectionsIP<=9);
#endif
	 
	 /* Maximum length of a nick name */
	 // unsigned int Conf_MaxNickLength;
#ifdef CONF_MAXNICKLENGTH
	 Conf_MaxNickLength = CONF_MAXNICKLENGTH;
#else
	 __SYMBOLIC(&Conf_MaxNickLength);
	 __ASSUME(Conf_MaxNickLength>=1);
	 __ASSUME(Conf_MaxNickLength<=9);
#endif
	
}

void symtest1(){
	
	int t;

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	t = 0;
		
	event_accept(client_fd1,t++);
	event_recv(client_fd1,"NICK martin\r\n",t++);
	event_recv(client_fd1,"USER kkma x x :kin keung ma\r\n",t++);
	event_send(client_fd1,t++);
	event_recv(client_fd1,"JOIN #channel\r\n",t++);
	event_end(t++);
}

int main(){

	symtest_Conf_Init = symtest_Conf_Init_impl;

	int argc = 2;
	char  argstr[] = "ngircd\0-n";
	char* argv[]   = {argstr,argstr+7};

	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->offsetout = 0;
	IOSIM_fd[1]->sym_fileout = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_fileout->contents = malloc(1);
	IOSIM_fd[1]->sym_fileout->stat.st_size = 1024;
	IOSIM_fd[1]->sym_fileout->stat.st_mode = S_IFSOCK;//

	sym_file_t* motdFile = IOSIM_addfile(MOTDFILE,0);
	motdFile->contents = strdup("Hello World!\n");
	motdFile->stat.st_size = strlen(motdFile->contents);

	stdout = IOSIM_fd[1];
	stderr = IOSIM_fd[1];

	symtest1();


	int ngircd_exit = main_ngircd(argc,argv);

	return 0;
}


