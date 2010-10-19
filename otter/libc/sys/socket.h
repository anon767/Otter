#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include <sys/uio.h>
#include <__otter/otter_fs.h>

#define SCM_RIGHTS 1234

#define SOCK_DGRAM 1 /* UDP */
#define SOCK_STREAM 2 /* TCP */
#define SOCK_SEQPACKET 3

#define SOL_SOCKET -1

/* these don't need to be bit flags in the spec, but it makes storing these values easier */
#define SO_ACCEPTCONN  0x00000001
#define SO_BROADCAST   0x00000002
#define SO_DEBUG       0x00000004
#define SO_DONTROUTE   0x00000008
#define SO_ERROR       0x00000010
#define SO_KEEPALIVE   0x00000020
#define SO_LINGER      0x00000040
#define SO_OOBINLINE   0x00000080
#define SO_RCVBUF      0x00000100
#define SO_RVCLOWAT    0x00000200
#define SO_RCVTIMEO    0x00000400
#define SO_REUSEADDR   0x00000800
#define SO_SNDBUF      0x00001000
#define SO_SNDLOWAT    0x00002000
#define SO_SNDTIMEO    0x00004000
#define SO_TYPE        0x00008000

#define MSG_CTRUNC 1
#define MSG_DONTROUTE 2
#define MSG_EOR 4
#define MSG_OOB 8
#define MSG_PEEK 16
#define MSG_TRUNC 32
#define MSG_WAITALL 64

#define AF_UNSPEC 0
#define AF_UNIX 1
#define AF_INET 2

#define PF_INET AF_INET

#define SHUT_RD 1
#define SHUT_WR 2
#define SHUT_RDWR 4

#define __otter_sock_ST_CLOSED 0
#define __otter_sock_ST_LISTEN 1
#define __otter_sock_ST_SYN_RCVD 2
#define __otter_sock_ST_SYN_SENT 3
#define __otter_sock_ST_ESTABLISHED 4
#define __otter_sock_ST_CLOSE_WAIT 5
#define __otter_sock_ST_LAST_ACK 6
#define __otter_sock_ST_FIN_WAIT_1 7
#define __otter_sock_ST_FIN_WAIT_2 8
#define __otter_sock_ST_CLOSING 9
#define __otter_sock_ST_TIME_WAIT 10
#define __otter_sock_ST_UDP 11

#define socklen_t unsigned int
#define sa_family_t unsigned short

#define __SOCKADDR_SHARED_LEN 16

struct sockaddr
{
	sa_family_t sa_family;
	char sa_data[14];
};

struct msghdr
{
	void* msg_name;
	socklen_t msg_namelen;
	struct iovec *msg_iov;
	int msg_iovlen;
	void* msg_control;
	socklen_t msg_controllen;
	int msg_flags;
};

struct cmsghdr
{
	socklen_t cmsg_len;
	int cmsg_level;
	int cmsg_type;
};

struct linger
{
	int l_onoff;
	int l_linger;
};

int socket(int domain, int type, int protocol);
int bind(int socket, const struct sockaddr *address, socklen_t address_len);
int setsockopt(int socket, int level, int option_name, const void *option_value, socklen_t option_len);
int getsockopt(int socket, int level, int option_name, void *option_value, socklen_t *option_len);

#endif
