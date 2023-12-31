#include "otter/otter_fs.h"
#include "otter/otter_scheduler.h"
#include "otter/otter_builtins.h"
#include "otter/multiotter_builtins.h"

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

/* Statically set the value of this global from Otter's sys/socket.h . Really,
	 this should probably be set in a function that starts up a process. */
unsigned short __otter_sock_free_port = 5000;

int socket(int domain, int type, int protocol)
{
	int fd = -1;

	switch(domain)
	{
		case AF_UNIX:
		case AF_INET:
		case AF_INET6:
			{
				struct __otter_fs_inode* inode = __otter_fs_init_new_socket();
				
				if(type == SOCK_DGRAM) /* UDP sockets are stateless */
				{
					((struct __otter_fs_sock_data*)inode->data)->state = __otter_sock_ST_UDP;
					((struct __otter_fs_sock_data*)inode->data)->recv_data = __otter_fs_init_new_pipe_data();
				}
				
				fd = __otter_fs_open_file(inode, O_RDWR);
				
				if(fd == -1) /* open failed */
				{
					__otter_fs_free_socket(inode);
					return -1;
				}
				
				((struct __otter_fs_sock_data*)inode->data)->addr->sa_family = domain;
				
				break;
			}
			
		case AF_UNSPEC:
		default:
			errno = EAFNOSUPPORT;
			return -1;
	}
	
	return(fd);
}

struct __otter_fs_sock_data* __otter_libc_get_sock_data_from_open_file(struct __otter_fs_open_file_table_entry* open_file) {
	__ASSERT(open_file->type == __otter_fs_TYP_SOCK);
	return open_file->inode->data;
}

struct __otter_fs_sock_data* __otter_libc_get_sock_data(int fd)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if(!open_file)
	{
		return NULL;
	}
	
	if(open_file->type != __otter_fs_TYP_SOCK)
	{
		errno = ENOTSOCK;
		return NULL;
	}

	return __otter_libc_get_sock_data_from_open_file(open_file);
}

int __otter_libc_setsockopt_sol_socket(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		/* TODO: impliment more of these */
		case SO_REUSEADDR:
		case SO_ACCEPTCONN:
		case SO_KEEPALIVE:
		case SO_DONTROUTE:
		case SO_OOBINLINE:
		case SO_BOUND:
			if(option_len < sizeof(int))
			{
				errno = EFAULT;
				return(-1);
			}
			
			if(*((int*)option_value))
			{
				sock->options |= option_name;
			}
			else
			{
				sock->options &= ~option_name;
			}
			return(0);
		case SO_LINGER:
			/* Linger is about waiting extra time at close() in the foreground to allow the network to flush extra packets.
			 * If linger is set close() will block as long at there is data unread in the receiving socket's recv_data.
			 */
			if(option_len < sizeof(struct linger))
			{
				errno = EFAULT;
				return(-1);
			}
			
			if(((struct linger*)option_value)->l_onoff) /* drop the time value and only keep the existance flag */
			{
				sock->options |= option_name;
			}
			else
			{
				sock->options &= ~option_name;
			}
			return(0);
		case SO_BROADCAST:
		case SO_DEBUG:
		case SO_ERROR:
		case SO_RCVBUF:
		case SO_RVCLOWAT:
		case SO_RCVTIMEO:
		case SO_SNDBUF:
		case SO_SNDLOWAT:
		case SO_SNDTIMEO:
		case SO_TYPE:
			/* TODO: impliment more of these */
		default:
			errno = EINVAL;
			return(-1);
	}
}

int __otter_libc_getsockopt_sol_socket(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		/* TODO: impliment more of these */
		case SO_REUSEADDR:
		case SO_ACCEPTCONN:
		case SO_BROADCAST:
		case SO_DEBUG:
		case SO_KEEPALIVE:
		case SO_DONTROUTE:
		case SO_OOBINLINE:
		case SO_BOUND:
			if(option_len < sizeof(int))
			{
				errno = EFAULT;
				return(-1);
			}
			
			*((int*)option_value) = (sock->options & option_name) ? 1 : 0;
			
			return(0);

		case SO_RCVBUF:
		case SO_SNDBUF:
			if(option_len < sizeof(int))
			{
				errno = EFAULT;
				return(-1);
			}
			
			*((int*)option_value) = __otter_fs_PIPE_SIZE;

			return(0);

		case SO_ERROR:
			/* There is no support for recoding socket errors, so report that there isn't an error. */
			return 0;
		case SO_LINGER:
			/* Linger is about waiting extra time at close() in the foreground to allow the network to flush extra packets.
			 * If linger is set close() will block as long at there is data unread in the receiving socket's recv_data.
			 */
			if(option_len < sizeof(struct linger))
			{
				errno = EFAULT;
				return(-1);
			}
			
			((struct linger*)option_value)->l_onoff = (sock->options & option_name) ? 1 : 0;
			((struct linger*)option_value)->l_linger = 16; /* arbitrary value */
			
			return 0;
		case SO_RVCLOWAT:
		case SO_RCVTIMEO:
		case SO_SNDLOWAT:
		case SO_SNDTIMEO:
		case SO_TYPE:
			/* TODO: impliment more of these */
		default:
			errno = EINVAL;
			return(-1);
	}
}

int __otter_libc_getsockopt_ipproto_tcp(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		case TCP_NODELAY:
			return(0);
		/* TODO: impliment more of these */
		default:
			errno = EINVAL;
			return(-1);
	}
}

int __otter_libc_setsockopt_ipproto_tcp(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		case TCP_NODELAY:
			return(0);
		/* TODO: impliment more of these */
		default:
			errno = EINVAL;
			return(-1);
	}
}

int setsockopt(int socket_fd, int level, int option_name, const void *option_value, socklen_t option_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	switch(level)
	{
		case SOL_SOCKET:
			return __otter_libc_setsockopt_sol_socket(sock, option_name, option_value, option_len);
		case IPPROTO_TCP:
			return __otter_libc_setsockopt_ipproto_tcp(sock, option_name, option_value, option_len);
		case IPPROTO_IP:
		case IPPROTO_ICMP:
		case IPPROTO_UDP:
			/* TODO: impliment somthing that makes sense so these (non-existant) protocol layers */
		default:
			errno = EINVAL;
			return(-1);
	}
	
	return(-1);
}

int getsockopt(int socket_fd, int level, int option_name, void *option_value, socklen_t *option_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	switch(level)
	{
		case SOL_SOCKET:
			return __otter_libc_getsockopt_sol_socket(sock, option_name, option_value, option_len);
		case IPPROTO_TCP:
			return __otter_libc_getsockopt_ipproto_tcp(sock, option_name, option_value, option_len);
		case IPPROTO_IP:
		case IPPROTO_ICMP:
		case IPPROTO_UDP:
			/* TODO: impliment somthing that makes sense so these (non-existant) protocol layers */
		default:
			errno = EINVAL;
			return(-1);
	}
	
	return(-1);
}

int bind(int socket_fd, const struct sockaddr *address, socklen_t address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	if(address == NULL)
	{
		errno = EFAULT;
		return(-1);
	}

	if(sock->addr->sa_family != address->sa_family) /* address families don't match */
	{
		errno = EAFNOSUPPORT;
		return(-1);
	}
	
	int l = 0;
	__otter_libc_getsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	if(l != 0)
	{
		errno = EINVAL;
		return(-1);
	}
	
	/* check that the socket is in an appropriate state for bind() */
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			break;
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			errno = EISCONN;
			return(-1);
		case __otter_sock_ST_UDP:
			break;
		default:
			__ASSERT(0);
	}
	
	memcpy(sock->addr, address, address_len);
	
	/* change any to loopback; assign port if it's needed */
	switch(sock->addr->sa_family)
	{
		case AF_UNIX:
			break;
		case AF_INET:
			{
				struct sockaddr_in* a = (struct sockaddr_in*)sock->addr;
				/* 0.0.0.0 */
				if(a->sin_addr.s_addr == 0)
				{
					a->sin_addr.s_addr = 0x0100007F; /* 127.0.0.1 */
				}
				
				if(a->sin_port == 0)
				{
					a->sin_port = __otter_sock_free_port;
					__otter_sock_free_port++;
				}
				else if(a->sin_port >= __otter_sock_free_port)
				{
					__otter_sock_free_port = a->sin_port + 1;
				}
			}
			break;
		case AF_INET6:
			{
				struct sockaddr_in6* a = (struct sockaddr_in6*)sock->addr;
				/* 0000:0000:0000:0000:0000:0000:0000:0000 */
				if(a->sin6_addr.s6_addr32[0] == 0 && a->sin6_addr.s6_addr32[1] == 0 && a->sin6_addr.s6_addr32[2] == 0 && a->sin6_addr.s6_addr32[3] == 0)
				{
					a->sin6_addr.s6_addr32[0] == 0x01000000; /* 0000:0000:0000:0000:0000:0000:0000:0001 */
				}
				
				if(a->sin6_port == 0)
				{
					a->sin6_port = __otter_sock_free_port;
					__otter_sock_free_port++;
				}
				else if(a->sin6_port >= __otter_sock_free_port)
				{
					__otter_sock_free_port = a->sin6_port + 1;
				}
			}
			break;
		case AF_UNSPEC:
		default:
			errno = EAFNOSUPPORT;
			return(-1);
	}

	/* look for a conflict with the address (already assigned and not SO_REUSEADDR) */
	int addr_used = 0;

	__otter_multi_begin_atomic();
	for(int i = 0; i < __otter_fs_MAX_OPEN_FILES; i++)
	{
		/* look for sockets. If the entry is not actually open or is not a socket, skip it. */
		struct __otter_fs_open_file_table_entry* open_file = &__otter_fs_open_file_table[i];
		if(open_file->openno == 0 || open_file->type != __otter_fs_TYP_SOCK)
			continue;
		
		struct __otter_fs_sock_data* other = __otter_libc_get_sock_data_from_open_file(open_file);
		if(other == NULL) /* this was not a socket */
		{
			continue;
		}
		
		__otter_libc_getsockopt_sol_socket(other, SO_BOUND, &l, sizeof(int));
		if(!l) /* sock is not bound */
		{
			continue;
		}
		
		if(memcmp(other->addr, sock->addr, address_len) == 0)
		{ /* a matching address was found, check to see that reuse is allowed */
			__otter_libc_getsockopt_sol_socket(other, SO_REUSEADDR, &l, sizeof(int));
			if(!l)
			{
				addr_used = 1;
				break;
			}
			__otter_libc_getsockopt_sol_socket(sock, SO_REUSEADDR, &l, sizeof(int));
			if(!l)
			{
				addr_used = 1;
				break;
			}
		}
	}
	__otter_multi_end_atomic();

	if(addr_used)
	{
		errno = EADDRINUSE;
		return(-1);
	}
	
	l = 1;
	__otter_libc_setsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	
	return(0);
}

int listen(int socket_fd, int backlog)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	if(backlog < 0)
	{
		backlog = 0;
	}
	
	int l = 0;
	__otter_libc_getsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	if(l != 1)
	{
		errno = EDESTADDRREQ;
		return(-1);
	}
	
	/* check that the socket is in an appropriate state for bind() */
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			break;
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_UDP: /* UDP dosn't listen */
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}
	
	sock->sock_queue = __otter_multi_gcalloc(backlog + 1, sizeof(struct __otter_fs_sock_data*));
	sock->backlog = backlog;
	sock->state = __otter_sock_ST_LISTEN;
	l = 1;
	__otter_libc_setsockopt_sol_socket(sock, SO_ACCEPTCONN, &l, sizeof(int));
	
	return(0);
	
}

void __otter_sock_pop_queue(struct __otter_fd_sock_data** q, int len)
{
	for(int i = 0; i < len - 1; i++)
	{
		q[i] = q[i + 1];
	}
	
	q[len - 1] = NULL;
}

int accept(int socket_fd, struct sockaddr *address, socklen_t *address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	/* check that the socket is in an appropriate state for accept() */
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_LISTEN:
			break;
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_UDP: /* UDP dosn't listen */
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}
	
	/* block until there is an incoming connection */
	__otter_multi_begin_atomic();
	while(sock->state == __otter_sock_ST_LISTEN)
	{
		if(sock->sock_queue[0])
		{

			struct __otter_fs_sock_data* sock_other = sock->sock_queue[0];
			if(sock_other->state != __otter_sock_ST_SYN_SENT) /* something happened to the other end of the socket */
			{
				__otter_sock_pop_queue(sock->sock_queue, sock->backlog + 1);
				break;
			}
			int fd = socket(sock->addr->sa_family, SOCK_STREAM, 0);
			if(fd == -1) /* failed to allocate new file descriptor */
			{
				__otter_sock_pop_queue(sock->sock_queue, sock->backlog + 1);
				sock_other->state = __otter_sock_ST_CLOSED;
				__otter_multi_end_atomic();
				return(-1);
			}
			
			struct __otter_fs_sock_data* sock2 = __otter_libc_get_sock_data(fd);
			sock2->state = __otter_sock_ST_ESTABLISHED;
			memcpy(sock2->addr, sock->addr, __SOCKADDR_SHARED_LEN);
			sock2->options = sock->options;
			int l = 0;
			__otter_libc_setsockopt_sol_socket(sock2, SO_ACCEPTCONN, &l, sizeof(int));
			sock2->sock_queue = __otter_multi_gcalloc(1, sizeof(struct __otter_fs_sock_data*));
			sock2->sock_queue[0] = sock_other;
			sock2->recv_data = __otter_fs_init_new_pipe_data();
			__otter_sock_pop_queue(sock->sock_queue, sock->backlog + 1);
			sock_other->sock_queue[0] = sock2;
			sock_other->state = __otter_sock_ST_ESTABLISHED;
			
			if(address)
			{
				memcpy(address, sock_other->addr, __SOCKADDR_SHARED_LEN);
				*address_len = __SOCKADDR_SHARED_LEN;
			}
			
			__otter_multi_end_atomic();
			return(fd);
		}
		else
		{
			__otter_multi_io_block(sock, sock->sock_queue);
		}
		__otter_multi_begin_atomic();
	}
	__otter_multi_end_atomic();

	return(-1);
}

void __otter_libc_flush_sock_queue(struct __otter_fs_sock_data* sock)
{
	for(int i = 0; i < sock->backlog + 1; i++)
	{
		if(sock->sock_queue[i] != NULL) /* there is something waiting here */
		{
			sock->sock_queue[i]->state = __otter_sock_ST_CLOSED;
			__otter_multi_gfree(sock->sock_queue[i]->sock_queue);
			sock->sock_queue[i]->sock_queue = NULL;
		}
	}
}

int connect(int socket_fd, const struct sockaddr *address, socklen_t address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	/* verify address is valid */
	if(address == NULL)
	{
		errno = EFAULT;
		return(-1);
	}
	
	/* if no address is bound, bind one */
	int l = 0;
	__otter_libc_getsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	if(l != 1)
	{
		switch(sock->addr->sa_family)
		{
			case AF_UNIX:
				break;
			case AF_INET:
			case AF_INET6:
				if(bind(socket_fd, sock->addr, address_len) == -1)
				{
					errno = EADDRNOTAVAIL;
					return(-1);
				}
				break;
			case AF_UNSPEC:
			default:
				errno = EAFNOSUPPORT;
				return(-1);
		}
	}
	
	/* check that the socket is in an appropriate state for connect() */
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			break;
		case __otter_sock_ST_LISTEN: /* switch to active open */
			/* stop listening */
			l = 0;
			__otter_libc_setsockopt_sol_socket(sock, SO_ACCEPTCONN, &l, sizeof(int));

			/* flush pending connections */
			__otter_libc_flush_sock_queue(sock);

			__otter_multi_gfree(sock->sock_queue);
			sock->sock_queue = NULL;
			
			break;
		case __otter_sock_ST_SYN_SENT:
			{
				struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(socket_fd);
				if(open_file->mode & O_NONBLOCK)
				{
					errno = EALREADY;
					return -1;
				}
			}
			/* else fall through */
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_UDP: /* UDP dosn't listen */
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}
	
	__otter_multi_begin_atomic();
	int q_size = 0;
	struct __otter_fs_sock_data* best_sock = NULL;
	for(int i = 0; i < __otter_fs_MAX_OPEN_FILES; i++)
	{
		/* look for listening sockets. If the entry is not actually open or is not a socket, skip it. */
		struct __otter_fs_open_file_table_entry* open_file = &__otter_fs_open_file_table[i];
		if(open_file->openno == 0 || open_file->type != __otter_fs_TYP_SOCK)
			continue;
		struct __otter_fs_sock_data* recv = __otter_libc_get_sock_data_from_open_file(open_file);
		if(recv->state != __otter_sock_ST_LISTEN)
			continue;
		
		/* check that the addresses match */
		if(memcmp(recv->addr, address, __SOCKADDR_SHARED_LEN) == 0)
		{
			int q2 = 0;
			for(int j = 0; i < recv->backlog + 1; j++)
			{
				if(recv->sock_queue[j] != NULL)
					q2++;
				else
					break;
			}
			
			/* choose the listening socket with teh shortest queue */
			if(best_sock == NULL || q_size > q2)
			{
				q_size = q2;
				best_sock = recv;
			}
		}
	}
	
	if(best_sock == NULL) /* no one was listening */
	{
		errno = ECONNREFUSED;
		__otter_multi_end_atomic();
		return(-1);
	}
	
	sock->state = __otter_sock_ST_SYN_SENT;
	sock->sock_queue = __otter_multi_gcalloc(1, sizeof(struct __otter_fs_sock_data*));
	sock->recv_data = __otter_fs_init_new_pipe_data();
	sock->sock_queue[0] = best_sock;
	
	for(int i = 0; i < best_sock->backlog + 1; i++)
	{
		if(best_sock->sock_queue[i] == NULL)
		{
			best_sock->sock_queue[i] = sock;
			__otter_multi_end_atomic();

			struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(socket_fd);
			if(sock->state == __otter_sock_ST_SYN_SENT && open_file->mode & O_NONBLOCK)
			{
				errno = EINPROGRESS;
				return -1;
			}
			
			__otter_multi_block_while_condition(sock->state == __otter_sock_ST_SYN_SENT, sock);
			
			if(sock->state == __otter_sock_ST_ESTABLISHED)
			{
				return(0);
			}
			else
			{
				errno = ECONNREFUSED;
				return(-1);
			}
		}
	}
	
	__otter_multi_end_atomic();
	errno = ECONNREFUSED;
	return(-1);
}

int __otter_libc_shutdown_sock_data(struct __otter_fs_sock_data* sock, int how)
{		
	/* check that the socket is in an appropriate state for shutdown() */
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
			errno = ENOTCONN;
			return(-1);
		case __otter_sock_ST_ESTABLISHED:
			{
				struct __otter_fs_sock_data* other = sock->sock_queue[0];
				switch(how)
				{
					case SHUT_RD:
						other->state = __otter_sock_ST_FIN_WAIT_1;
						sock->state = __otter_sock_ST_CLOSE_WAIT;
						other->state = __otter_sock_ST_FIN_WAIT_2;
						return(0);
					case SHUT_WR:
						sock->state = __otter_sock_ST_FIN_WAIT_1;
						other->state =  __otter_sock_ST_CLOSE_WAIT;
						sock->state = __otter_sock_ST_FIN_WAIT_2;
						return(0);
					case SHUT_RDWR:
						sock->state = __otter_sock_ST_FIN_WAIT_1;
						other->state = __otter_sock_ST_FIN_WAIT_1;
						sock->state = __otter_sock_ST_CLOSING;
						other->state = __otter_sock_ST_CLOSING;
						sock->state = __otter_sock_ST_TIME_WAIT;
						other->state = __otter_sock_ST_TIME_WAIT;
						sock->state = __otter_sock_ST_CLOSED;
						other->state = __otter_sock_ST_CLOSED;
						sock->sock_queue[0] = NULL;
						other->sock_queue[0] = NULL;
						return(0);
					default:
						errno = EINVAL;
						return(-1);
				}
			}
			break;
		case __otter_sock_ST_CLOSE_WAIT:
			{
				struct __otter_fs_sock_data* other = sock->sock_queue[0];
				switch(how)
				{
					case SHUT_RD:
						return(0);
					case SHUT_WR:
					case SHUT_RDWR:
						sock->state = __otter_sock_ST_LAST_ACK;
						other->state = __otter_sock_ST_TIME_WAIT;
						sock->state = __otter_sock_ST_CLOSED;
						other->state = __otter_sock_ST_CLOSED;
						sock->sock_queue[0] = NULL;
						other->sock_queue[0] = NULL;
						return(0);
					default:
						errno = EINVAL;
						return(-1);
				}
			}
			break;
		case __otter_sock_ST_LAST_ACK:
			errno = ENOTCONN;
			return(-1);
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
			{
				struct __otter_fs_sock_data* other = sock->sock_queue[0];
				switch(how)
				{
					case SHUT_WR:
						return(0);
					case SHUT_RD:
					case SHUT_RDWR:
						other->state = __otter_sock_ST_LAST_ACK;
						sock->state = __otter_sock_ST_TIME_WAIT;
						sock->state = __otter_sock_ST_CLOSED;
						other->state = __otter_sock_ST_CLOSED;
						sock->sock_queue[0] = NULL;
						other->sock_queue[0] = NULL;
						return(0);
					default:
						errno = EINVAL;
						return(-1);
				}
			}
			break;
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			errno = ENOTCONN;
			return(-1);
		case __otter_sock_ST_UDP:
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}

	__ASSERT(0);	
	return (-1);
}

int getpeername(int socket_fd, struct sockaddr *address, socklen_t *address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	/* verify address is valid */
	if(address == NULL)
	{
		errno = EFAULT;
		return(-1);
	}
	
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			errno = EINVAL;
			return(-1);
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
			errno = ENOTCONN;
			return(-1);
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
			memcpy(address, sock->sock_queue[0]->addr, __SOCKADDR_SHARED_LEN);
			*address_len = __SOCKADDR_SHARED_LEN;
			break;
		case __otter_sock_ST_UDP: /* UDP dosn't listen */
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}
	
	return(0);
}

int getsockname(int socket_fd, struct sockaddr *address, socklen_t *address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	/* verify address is valid */
	if(address == NULL)
	{
		errno = EFAULT;
		return(-1);
	}
	
	memcpy(address, sock->addr, __SOCKADDR_SHARED_LEN);
	*address_len = __SOCKADDR_SHARED_LEN;

	return(0);
}

ssize_t recv(int socket_fd, void *buf, size_t num, int flags)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(socket_fd);
	if(!open_file)
	{
		return -1;
	}
	
	return __otter_libc_recv_socket(open_file, buf, num, flags);
}

ssize_t __otter_libc_recv_socket(struct __otter_fs_open_file_table_entry* open_file, void *buf, size_t num, int flags)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data_from_open_file(open_file);
	if(!sock)
	{
		return(-1);
	}
	
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
			return 0;
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
		case __otter_sock_ST_UDP:
			/* can't read() in these states */
			errno = ENOTCONN;
			return(-1);
			break;

		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
			{
				/* Unless O_NONBLOCK is set, block until data becomes available. Then read the data. */
				if (__otter_fs_pipe_is_empty(sock->recv_data))
				{
					if(open_file->mode & O_NONBLOCK)
					{
						errno = EAGAIN;
						return -1;
					}
					
					__otter_multi_block_while_condition(
						__otter_fs_pipe_is_empty(sock->recv_data) &&
						(
							sock->state == __otter_sock_ST_ESTABLISHED || 
							sock->state == __otter_sock_ST_FIN_WAIT_1 || 
							sock->state == __otter_sock_ST_FIN_WAIT_2
						)
						,
						sock->recv_data, sock);
						
					/* socket can't be read from anymore? */
					/* TODO: make this return -1 if the connection is dropped instead of cleanly shutdown */
					switch(sock->state)
					{
						case __otter_sock_ST_CLOSED:
						case __otter_sock_ST_LISTEN:
						case __otter_sock_ST_SYN_RCVD:
						case __otter_sock_ST_SYN_SENT:
						case __otter_sock_ST_CLOSE_WAIT:
						case __otter_sock_ST_LAST_ACK:
						case __otter_sock_ST_CLOSING:
						case __otter_sock_ST_TIME_WAIT:
						case __otter_sock_ST_UDP:
							return(0);

						case __otter_sock_ST_ESTABLISHED:
						case __otter_sock_ST_FIN_WAIT_1:
						case __otter_sock_ST_FIN_WAIT_2:
							break;
					}
				}
				/* This is a TOCTTOU problem on whether the pipe is empty, but opengroup says:
					'The behavior of multiple concurrent reads on the same pipe, FIFO, or terminal device is unspecified.'
					so this will (hopefully) never cause trouble. */

				if(flags & MSG_PEEK)
					return __otter_libc_pread_pipe_data(sock->recv_data, buf, num);
				else {
					return __otter_libc_read_pipe_data(sock->recv_data, buf, num);
				}
			}
	}
	__ASSERT(0);
	abort();
}

int shutdown(int socket_fd, int how)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	return __otter_libc_shutdown_sock_data(sock, how);
}

ssize_t send(int socket_fd, const void *buf, size_t num, int flags)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	return __otter_libc_write_socket(sock, buf, num);
}
