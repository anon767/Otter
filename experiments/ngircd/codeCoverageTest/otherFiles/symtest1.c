#define main main_ngircd
#include "../ngircd_comb.c"
#undef main

#include <stdio.h>
#include <string.h>
#include <iosim.h>


void symtest_Conf_Init_impl(){
	 /* Name ("Nick") of the servers */
	 // char Conf_ServerName[CLIENT_ID_LEN];
	 strcpy(Conf_ServerName,"@Conf_ServerName");
	 
	 /* Server info text */
	 // char Conf_ServerInfo[CLIENT_INFO_LEN];
	 strcpy(Conf_ServerInfo,"@Conf_ServerInfo");
	 
	 /* // server passwort */
	 // char Conf_ServerPwd[CLIENT_PASS_LEN];
	 strcpy(Conf_ServerPwd,"@Conf_ServerPwd");
	 
	 /* Administrative information */
	 // char Conf_ServerAdmin1[CLIENT_INFO_LEN];
	 // char Conf_ServerAdmin2[CLIENT_INFO_LEN];
	 // char Conf_ServerAdminMail[CLIENT_INFO_LEN];
	 strcpy(Conf_ServerAdmin1,"@Conf_ServerAdmin1");
	 strcpy(Conf_ServerAdmin2,"@Conf_ServerAdmin2");
	 strcpy(Conf_ServerAdminMail,"@Conf_ServerAdminMail");
	 
	 /* File with MOTD text */
	 // char Conf_MotdFile[FNAME_LEN];
	 strcpy(Conf_MotdFile,"@Conf_MotdFile");
	 
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
	 __SYMBOLIC(&Conf_UID);
	 __SYMBOLIC(&Conf_GID);
	 
	 /* A directory to chroot() in */
	 // char Conf_Chroot[FNAME_LEN];
	 
	 /* File with PID of daemon */
	 // char Conf_PidFile[FNAME_LEN];
	 
	 /* Timeouts for PING and PONG */
	 __SYMBOLIC(&Conf_PingTimeout);
	 __SYMBOLIC(&Conf_PongTimeout);
	 
	 /* Seconds between connect attempts to other servers */
	 // int Conf_ConnectRetry;
	 __SYMBOLIC(&Conf_ConnectRetry);
	 
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
	 __SYMBOLIC(&Conf_OperCanMode);
	 
	 /* Disable all DNS functions? */
	 // bool Conf_NoDNS;
	 __SYMBOLIC(&Conf_NoDNS);
	 
	 /* listen for incoming ipv6 connections if OS supports it (default: yes)? */
	 // bool Conf_ListenIPv6;
	 __SYMBOLIC(&Conf_ListenIPv6);
	 
	 /* listen for incoming ipv4 connections if OS supports it (default: yes)? */
	 // bool Conf_ListenIPv4;
	 __SYMBOLIC(&Conf_ListenIPv4);
	 
	 /*
	  * try to connect to remote systems using the ipv6 protocol,
	  * if they have an ipv6 address? (default yes)
	  */
	 // bool Conf_ConnectIPv6;
	 __SYMBOLIC(&Conf_ConnectIPv6);
	 
	 /* same as above, but for ipv4 hosts, default: yes  */
	 // bool Conf_ConnectIPv4;
	 __SYMBOLIC(&Conf_ConnectIPv4);
	 
	 /* If an IRC op gives chanop privileges without being a chanop,
	  * ircd2 will ignore the command. This enables a workaround:
	  * It masks the command as coming from the server */
	 // bool Conf_OperServerMode;
	 __SYMBOLIC(&Conf_OperServerMode);
	 
	 /* Maximum number of connections to this server */
	 // long Conf_MaxConnections;
//	 __SYMBOLIC(&Conf_MaxConnections);
//	 __ASSUME(Conf_MaxConnections>=0);
	 
	 /* Maximum number of channels a user can join */
	 // int Conf_MaxJoins;
	 __SYMBOLIC(&Conf_MaxJoins);
	 __ASSUME(Conf_MaxJoins>=0);
	 
	 /* Maximum number of connections per IP address */
	 // int Conf_MaxConnectionsIP;
	 __SYMBOLIC(&Conf_MaxConnectionsIP);
	 __ASSUME(Conf_MaxConnectionsIP>=0);
	 
	 /* Maximum length of a nick name */
	 // unsigned int Conf_MaxNickLength;
	 __SYMBOLIC(&Conf_MaxNickLength);
	 __ASSUME(Conf_MaxNickLength>=0);
	
}

void symtest1(){

	// setup client(s)
	int client_fd1 = socket(0,0,0);

	// setup listen socket
	listen_queue[0] = client_fd1;
	listen_queue_size = 1;
		
	event_accept(client_fd1,0);
	event_recv(client_fd1,"NICK nick",1);
	event_recv(client_fd1,"USER user x x :x",2);
	event_send(client_fd1,3);
	event_end(4);
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

	stdout = IOSIM_fd[1];
	stderr = IOSIM_fd[1];

	symtest1();


	int ngircd_exit = main_ngircd(argc,argv);

	return 0;
}


