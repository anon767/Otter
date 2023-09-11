/* Mock functions are defined in this file, and included with all source files via gcc -include (or cilly -include)
 * 
 * For each function to mock, #define the mock object and declare its prototype:
 * #define fn(i) mock_fn(i)
 * int mock_fn(int);
 * 
 * or alternatively, the following handles any number of arguments:
 * #define fn(...) mock_fn(__VA_ARGS__) 
 * int mock_fn(int);
 */
#ifndef SYMTEST_MOCK_H
#define SYMTEST_MOCK_H

/*
 * constants for server setting
 */
#define __TOSTRING(X)	# X
int SERVER_GID;
int SERVER_UID;
char SERVER_IP[100];
char SERVER_NAME[100];
int SERVER_NETWORK;

#ifndef		__NEW_TEST_VERSION__
#define MAX_CLIENT		10
char*	CLIENT_ADDR[MAX_CLIENT];
char*	CLIENT_NICK[MAX_CLIENT]; //	"NICK kkma\r\n"
char*	CLIENT_USER[MAX_CLIENT]; //	"USER kinkeung hostname servername :kin ma\r\n"
char*	CLIENT_JOIN[MAX_CLIENT]; 
char*	CLIENT_PRIVMSG[MAX_CLIENT]; 
char*	CLIENT_PART[MAX_CLIENT]; 
#endif

/*
 * constants for client setting
 */

/*
 * set which mock functions are included
 */
// grp
#define __HAVE_getgrgid__	1			// 11
#define __HAVE_getgrnam__	1			// 7
// inet
#define __HAVE_inet_aton__	1			// 14
#define	__HAVE_inet_ntoa__	1			// 20
// netdb
#define	__HAVE_gethostbyname__	1		// 13
#define	__HAVE_gethostbyaddr__	0
// poll
#define	__HAVE_poll__	1				// 21
// pwd
#define	__HAVE_getpwnam__	1			// 6
#define	__HAVE_endpwent__	0
#define	__HAVE_getpwuid__	1			// 10
// socket
#define	__HAVE_socket__	1				// 15
#define	__HAVE_setsockopt__	1			// 16
#define	__HAVE_bind__	1				// 18
#define	__HAVE_listen__	1				// 19
#define	__HAVE_accept__	1				// 22
// time
#define	__HAVE_time__	1				// 2
#define	__HAVE_localtime__	1			// 3
#define	__HAVE_strftime__	1			// 4
// unistd
#define	__HAVE_getpid__	1				// 5
#define	__HAVE_getgid__	1				// 8
#define	__HAVE_getuid__	1				// 9
#define	__HAVE_fork__	1				// 24
#define	__HAVE_gethostname__	1		// 12
#define	__HAVE_pipe__	1				// 23
#define	__HAVE_read__	1				// 26
#define	__HAVE_close__	1				// 25
// posix
#define __HAVE_umask__	1				// 1
// fcntl
#define __HAVE_fcntl__	1				// 17


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#endif
