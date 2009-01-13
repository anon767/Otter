#include "symtest.h"
#include "symtest_mock.h"
#include <iosim.h>
#include <stdlib.h>
#include <string.h>
// poll
#include <poll.h>
// accept
#include <sys/socket.h>
// read and write
#include <unistd.h>

/* ngircd_comb.c entry functions */
extern symtest_actual_main(int argc,char** argv);

#define NICK_FORMAT	"NICK XXX\r\n"

void sym_test_1 (void* param) {
	IO_BUF* ConfFile_in = IOSIM_newbuf(
			-1,
			"[Global]\n" \
			"Name = irc.the.net\n" \
			"Info = Server Info Text\n" \
			"MaxConnections = 10\n" \
			"ServerUID = " __TOSTRING(SERVER_UID) "\n"  \
			"ServerGID = " __TOSTRING(SERVER_GID) "\n"
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
	
	char* SYM_NICK[MAX_CLIENT];

	for(int j=0;j<=1;j++)
	{	
		SYM_NICK[j] = malloc(strlen(NICK_FORMAT)+1);
		strcpy(SYM_NICK[j],NICK_FORMAT);
		for(int i=0;i<3;i++)
		{
			char symbol = __SYMBOLIC(1); // symbolic
			__ASSUME(symbol>='a',symbol<='z');
			// symbol != i   => prevent branching since ircd has a "builtin" client of name "i..."
			if(i==0) __ASSUME(symbol!='i');
			SYM_NICK[j][5+i] = symbol;
		}
		CLIENT_ADDR[j] = "1.2.3.4";
		CLIENT_NICK[j] = SYM_NICK[j];
		CLIENT_USER[j] = "USER kinkeung 143.89.0.1 192.168.0.1 :kin ma\r\n";
	}
	{
		char SYM_JOIN[] = "JOIN #xxx\r\n";
		for(int i=0;i<3;i++)
		{
			char symbol = __SYMBOLIC(1); // symbolic
			__ASSUME(symbol>='a',symbol<='z');
			SYM_JOIN[6+i] = symbol;
		}
		CLIENT_JOIN[0] = SYM_JOIN;
	}

	symtest_actual_main(argc,argv);
}

int main() {
    symtest_run_test(sym_test_1, NULL);
    return 0;
}

#if __HAVE_poll__
int poll(struct pollfd fds[], nfds_t nfds, int timeout){
	static int call_count = 0;
	call_count ++;

	for(int i=0;i<nfds;i++)
		fds[i].revents = 0;

	switch(call_count){
	case 1:
		// the listen socket is 3
		fds[4].revents = POLLPRI;
		return 1;
	case 2:
		// the client socket is 4
		// the clinet wanna send NICK
		fds[5].revents = POLLIN;
		return 1;
	case 3:
		// the client socket is 4
		// the clinet wanna send USER
		fds[5].revents = POLLIN;
		return 1;
	case 4:
		// now the server should respond
		assert(fds[5].events == POLLOUT);
		fds[5].revents = POLLOUT;
		return 1;
	case 5:
		// the listen socket is 3
		fds[4].revents = POLLPRI;
		return 1;
	case 6:
		// the client socket is 4
		// the clinet wanna send NICK
		fds[9].revents = POLLIN;
		return 1;
	case 7:
		// the client socket is 4
		// the clinet wanna send USER
		fds[9].revents = POLLIN;
		return 1;
	case 8:
		// now the server should respond
		assert(fds[9].events == POLLOUT);
		fds[9].revents = POLLOUT;
		return 1;

	case 9:
		// the client socket is 4
		// the clinet wanna send JOIN
		fds[5].revents = POLLIN;
		return 1;
	case 10:
		// now the server should respond
		assert(fds[5].events == POLLOUT);
		fds[5].revents = POLLOUT;
		return 1;
	default: 
	
		//__COMMENT("LEVEL2");
		//for(int j=0;j<10;j++)
		//	__EVALSTR(write_output[j],1000);


		//__COMMENT("LEVEL3");
		//for(int j=5;j<7;j++)
		//	for(int k=0;k<100;k++) //first 100 char
		//		__EVAL(write_output[j][k]);
		__COMMENT("DONE");
		exit(1);
	}
	return -1;
}
#endif

#if __HAVE_accept__
int accept (int socket, struct sockaddr *address,
		                                 socklen_t *address_len){
	static int call_count = 0;
	call_count ++;

	switch(call_count){
	case 1:
		assert(socket==4);
		strcpy(address->sa_data,CLIENT_ADDR[0]);
		*address_len  = strlen(CLIENT_ADDR[0]);
		return IOSIM_newfd(); //5
	case 2:
		assert(socket==4);
		strcpy(address->sa_data,CLIENT_ADDR[1]);
		*address_len  = strlen(CLIENT_ADDR[1]);
		return IOSIM_newfd(); //8?
	case 3:
	default: exit(1);
	}
	return -1;
}
#endif


#if __HAVE_read__
ssize_t read(int fildes, void *buf, size_t nbyte){
	static int call_count = 0;
	call_count ++;
	switch(call_count){
	case 1:
		// SEND NICK/USER
		assert(fildes==5);
		strcpy(buf,CLIENT_NICK[0]);
		return strlen(CLIENT_NICK[0]);
	case 2:
		// SEND USER/NICK
		assert(fildes==5);
		strcpy(buf,CLIENT_USER[0]);
		return strlen(CLIENT_USER[0]);
	case 3:
		// SEND NICK/USER
		assert(fildes==9);
		strcpy(buf,CLIENT_NICK[1]);
		return strlen(CLIENT_NICK[1]);
	case 4:
		// SEND USER/NICK
		assert(fildes==9);
		strcpy(buf,CLIENT_USER[1]);
		return strlen(CLIENT_USER[1]);
	case 5:
		// SEND JOIN
		assert(fildes==5);
		strcpy(buf,CLIENT_JOIN[0]);
		return strlen(CLIENT_JOIN[0]);
	case 4:
	default: exit(1);
	}
	return -1;
}	
#endif


// Nothing but just output anything received from server
ssize_t write(int fildes, const void *buf, size_t nbyte){
	static int call_count = 0;

	call_count ++;
	switch(call_count){
	case 1:
	case 2:
	case 3:
	case 4:
	default: 
		//assert(fildes==5);
		//strncat(write_output[fildes],buf,nbyte);
		//__EVALSTR(write_output[fildes]);
		__EVALSTR(buf,nbyte);
		return nbyte;
	}
	return -1;
}

