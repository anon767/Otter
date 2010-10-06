#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include<sys/uio.h>

#define socklen_t unsigned int
#define sa_family_t unsigned int

#define SCM_RIGHTS 1234

#define SOCK_DGRAM 1 /* UDP */
#define SOCK_STREAM 2 /* TCP */
#define SOCK_SEQPACKET 3

#define SOL_SOCKET -1

#define SO_ACCEPTCONN 1
#define SO_BROADCAST 2
#define SO_DEBUG 3
#define SO_DONTROUTE 4
#define SO_ERROR 5
#define SO_KEEPALIVE 6
#define SO_LINGER 7
#define SO_OOBINLINE 8
#define SO_RCVBUF 9
#define SO_RVCLOWAT 10
#define SO_RCVTIMEO 11
#define SO_REUSEADDR 12
#define SO_SNDBUF 13
#define SO_SNDLOWAT 14
#define SO_SNDTIMEO 15
#define SO_TYPE 16

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

#define SHUT_RD 1
#define SHUT_WR 2
#define SHUT_RDWR 4

struct sockaddr
{
	sa_family_t sa_family;
	char sa_data[];
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

#endif
