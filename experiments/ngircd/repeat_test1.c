//
// Test: a person sends many messages to a channel
//
#include "symtest.h"
#include "symtest_mock.h"
#include "mock/iosim.h"
#include <stdlib.h>
#include <string.h>
// poll
#include <poll.h>
// accept
#include <sys/socket.h>
// read and write
#include <unistd.h>
#include "ngircd-header.h"

/* ngircd_comb.c entry functions */
extern symtest_actual_main(int argc,char** argv);

extern COMMAND My_Commands[48];


#define MAX_CLIENT		100
#define NUM_CLIENT 		1
#define NUM_MESSAGE 	20
char	CLIENT_ADDR[MAX_CLIENT][1024];
char	CLIENT_NICK[MAX_CLIENT][1024]; //	"NICK kkma\r\n"
char	CLIENT_USER[MAX_CLIENT][1024]; //	"USER kinkeung hostname servername :kin ma\r\n"
char	CLIENT_JOIN[MAX_CLIENT][1024]; //	"JOIN #group\r\n"
char	CLIENT_PRIVMSG[MAX_CLIENT][1024]; //	"PRIVMSG #group :Hello\r\n"


void sym_test_1 (void* param) {
	IO_BUF* ConfFile_in = IOSIM_newbuf(
			-1,
			"[Global]\n" \
			"Name = irc.the.net\n" \
			"Info = Server Info Text\n" \
			"MaxConnections = 6\n" \
			"ServerUID = " __TOSTRING(SERVER_UID) "\n"  \
			"ServerGID = " __TOSTRING(SERVER_GID) "\n"  \
			"PingTimeout = 2147483647\n" \
			"PongTimeout = 2147483647\n" \
			"MaxConnectionsIP = 0\n" \
			""
			);
	IOSIM_addfile("/usr/local/etc/ngircd.conf",ConfFile_in);

	int argc = 2;
	char  argstr[] = "ngircd\0-n";
	char* argv[]   = {argstr,argstr+7};

	SERVER_GID	 = 65534;
	SERVER_UID	 = 65534;
	strcpy(SERVER_IP,"1.2.3.4");
	strcpy(SERVER_NAME,"sym.com");
	SERVER_NETWORK = 4670;
	

	// registration
	for(int j=0;j<NUM_CLIENT;j++)
	{	
		char name = __SYMBOLIC();
		__ASSUME(name >= 'a', name <= 'z');
		char group_name = __SYMBOLIC();
		__ASSUME(group_name >= 'a', group_name <= 'z');
		strcpy(CLIENT_NICK[j],"NICK CCX\r\n");
		CLIENT_NICK[j][7] = name;
		CLIENT_ADDR[j][0] = 0;
		CLIENT_ADDR[j][1] = 0;
		CLIENT_ADDR[j][2] = 1+j;
		CLIENT_ADDR[j][3] = 0;
		CLIENT_ADDR[j][4] = 0;
		CLIENT_ADDR[j][5] = 127;
		strcpy(CLIENT_USER[j] , "USER kinkeung 143.89.0.1 192.168.0.1 :kin ma\r\n");
		strcpy(CLIENT_JOIN[j] , "JOIN #g\r\n");
		strcpy(CLIENT_PRIVMSG[j] , "PRIVMSG #g :Hello\r\n");

		CLIENT_JOIN[j][6] = group_name;
		CLIENT_PRIVMSG[j][9] = group_name;
	}
	

	symtest_actual_main(argc,argv);
}

int main() {
    symtest_run_test(sym_test_1, NULL);
    return 0;
}

// GLOBALS
int accept_call_count = 0;
int read_call_count = 0;
int poll_call_count = 0;
extern current_time;

int poll(struct pollfd fds[], nfds_t nfds, int timeout){
	poll_call_count ++;
	int this_poll_call_count = poll_call_count;


	for(int i=0;i<nfds;i++)
		fds[i].revents = 0;

	for(int i=0;i<NUM_CLIENT;i++){
		int client_fd = 5+4*i;
		if((--this_poll_call_count)==0){
			// the listen socket is 4
			fds[4].revents = POLLPRI;
			return 1;
		}else if((--this_poll_call_count)==0){
			// the clinet wanna send NICK
			fds[client_fd].revents = POLLIN;
			return 1;
		}else if((--this_poll_call_count)==0){
			// the clinet wanna send USER
			fds[client_fd].revents = POLLIN;
			return 1;
		}else if((--this_poll_call_count)==0){
			// now the server should respond
			__ASSERT(fds[client_fd].events == POLLOUT);
			fds[client_fd].revents = POLLOUT;
			return 1;
		}else if((--this_poll_call_count)==0){
			// the clinet wanna send JOIN
			fds[client_fd].revents = POLLIN;
			return 1;
		}else if((--this_poll_call_count)==0){
			// now the server should respond
			__ASSERT(fds[client_fd].events == POLLOUT);
			fds[client_fd].revents = POLLOUT;
			return 1;
		}else{
			// Main loop of the repeating send-privmsg events
			for(int j=0;j<NUM_MESSAGE;j++){
				if((--this_poll_call_count)==0){
					for(int k=0;k<48;k++)
						My_Commands[k].lcount = __SYMBOLIC_STATIC(k);
					for(int k=0;k<6;k++){
						// MaxConnections = 6

						// The following will branch executions:
						//My_Connections[k].delaytime = __SYMBOLIC_STATIC(k);

						// The followings are needed:
						My_Connections[k].lastdata = __SYMBOLIC_STATIC(k);
						My_Connections[k].lastprivmsg = __SYMBOLIC_STATIC(k);
						My_Connections[k].lastping = __SYMBOLIC_STATIC(k);
						My_Connections[k].bytes_in = __SYMBOLIC_STATIC(k);
						My_Connections[k].msg_in = __SYMBOLIC_STATIC(k);
					}
					
					int tmp_poll_call_count = poll_call_count;
					int tmp_read_call_count = read_call_count;
					int tmp_current_time = current_time;
					poll_call_count = __SYMBOLIC_STATIC();
					read_call_count = __SYMBOLIC_STATIC();
					current_time = __SYMBOLIC_STATIC();
					__ASSERT_EQUAL_STATE(1);
					poll_call_count = tmp_poll_call_count;
					read_call_count = tmp_read_call_count;
					current_time = tmp_current_time;
					

				 	// the clinet wanna send PRIVMSG
				 	fds[client_fd].revents = POLLIN;
				 	return 1;
				}
			}
		}
	}
	{
		char output[100]; sprintf(output,"DONE");
		__EVALSTR(output,strlen(output));
		exit(1);
	}
	return -1;
}

int accept (int socket, struct sockaddr *address,
		                                 socklen_t *address_len){
	accept_call_count ++;
	int this_accept_call_count = accept_call_count;

	for(int i=0;i<NUM_CLIENT;i++)
	if((--this_accept_call_count)==0){
		__ASSERT(socket==4);
		memcpy(address->sa_data,CLIENT_ADDR[i],6);
		*address_len  = strlen(CLIENT_ADDR[i]);
		int newfd = IOSIM_newfd(); //5
		__ASSERT(newfd==5+4*i);
		return newfd;
	}
	return -1;
}

ssize_t read(int fildes, void *buf, size_t nbyte){
	read_call_count ++;
	int this_read_call_count = read_call_count;
	for(int i=0;i<NUM_CLIENT;i++){
		int client_fd = 5+4*i;
		if((--this_read_call_count)==0){
			// SEND NICK/USER
			__ASSERT(fildes==client_fd);
			strcpy(buf,CLIENT_NICK[i]);
			return strlen(CLIENT_NICK[i]);
		}else if((--this_read_call_count)==0){
			// SEND USER/NICK
			__ASSERT(fildes==client_fd);
			strcpy(buf,CLIENT_USER[i]);
			return strlen(CLIENT_USER[i]);
		}else if((--this_read_call_count)==0){
			// SEND JOIN
			__ASSERT(fildes==client_fd);
			strcpy(buf,CLIENT_JOIN[i]);
			return strlen(CLIENT_JOIN[i]);
		}
		else{
			for(int j=0;j<NUM_MESSAGE;j++){
				if((--this_read_call_count)==0){
					// SEND PRIVMSG
					__ASSERT(fildes==client_fd);
					strcpy(buf,CLIENT_PRIVMSG[i]);
					return strlen(CLIENT_PRIVMSG[i]);
				}
			}
		}
	}
	return -1;
}	


// Nothing but just output anything received from server
ssize_t write(int fildes, const void *buf, size_t nbyte){
	char output[1000];
	((char*)buf)[nbyte] = 0;
	sprintf(output,"<%d> %s",fildes,buf); // TODO: set end of buf == '\0'
	__EVALSTR(output,strlen(output));
	return nbyte;
}

