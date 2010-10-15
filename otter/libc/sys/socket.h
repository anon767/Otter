#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include <sys/uio.h>
#include <__otter/otter_fs.h>

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

void __otter_libc_free_socket(struct __otter_fs_inode* inode);

#endif
