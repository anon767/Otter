#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include <sys/uio.h>
#include <otter/otter_fs.h>

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

#define SO_BOUND       0x00010000

#define MSG_CTRUNC 1
#define MSG_DONTROUTE 2
#define MSG_EOR 4
#define MSG_OOB 8
#define MSG_PEEK 16
#define MSG_TRUNC 32
#define MSG_WAITALL 64

#define AF_UNSPEC 0
#define AF_UNIX 1
/* http://www.delorie.com/gnu/docs/glibc/libc_305.html says of AF_UNIX:
   'This is a synonym for AF_LOCAL. Although AF_LOCAL is mandated by POSIX.1g, AF_UNIX is portable to more systems.' */
#define AF_LOCAL AF_UNIX
#define AF_INET 2
#define AF_INET6 3

#define PF_UNSPEC AF_UNSPEC
#define PF_UNIX AF_UNIX
#define PF_INET AF_INET
#define PF_INET6 AF_INET6

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

typedef unsigned int socklen_t;
typedef unsigned short sa_family_t;

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
int bind(int socket_fd, const struct sockaddr *address, socklen_t address_len);
int listen(int socket_fd, int backlog);
int accept(int socket_fd, struct sockaddr *address, socklen_t *address_len);
int connect(int socket_fd, const struct sockaddr *address, socklen_t address_len);
int shutdown(int socket_fd, int how);

ssize_t recv(int socket, void *buffer, size_t length, int flags);
ssize_t send(int socket, const void *buffer, size_t length, int flags);

int setsockopt(int socket_fd, int level, int option_name, const void *option_value, socklen_t option_len);
int getsockopt(int socket_fd, int level, int option_name, void *option_value, socklen_t *option_len);

int getpeername(int socket, struct sockaddr *address, socklen_t *address_len);
int getsockname(int socket, struct sockaddr *address, socklen_t *address_len);

unsigned short __otter_sock_free_port;
struct __otter_fs_sock_data* __otter_libc_get_sock_data(int fd);
struct __otter_fs_sock_data* __otter_libc_get_sock_data_from_open_file(struct __otter_fs_open_file_table_entry* open_file);
void __otter_libc_flush_sock_queue(struct __otter_fs_sock_data* sock);
ssize_t __otter_libc_read_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num, int nonblocking);
ssize_t __otter_libc_pread_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num,
	int nonblocking);
ssize_t __otter_libc_write_socket(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num);
int __otter_libc_shutdown_sock_data(struct __otter_fs_sock_data* sock, int how);

#endif
