#include <sys/socket.h>
#include <__otter/otter_fs.h>
#include <fcntl.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>

int __otter_libc_socket(int domain, int type, int protocol)
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
				}
				
				((struct __otter_fs_sock_data*)inode->data)->addr->sa_family = domain;
				
				break;
			}
			
		case AF_UNSPEC:
		default:
			errno = EAFNOSUPPORT;
			break;
	}
	
	return(fd);
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
	
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)inode->data;
	
	return(sock);
}

int __otter_libc_setsockopt_sol_socket(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		/* TODO: impliment more of these */
		case SO_REUSEADDR:
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
		case SO_ACCEPTCONN:
		case SO_BROADCAST:
		case SO_DEBUG:
		case SO_DONTROUTE:
		case SO_ERROR:
		case SO_KEEPALIVE:
		case SO_LINGER:
		case SO_OOBINLINE:
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
		case SO_LINGER:
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

int __otter_libc_setsockopt(int socket, int level, int option_name, const void *option_value, socklen_t option_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket);
	if(!sock)
	{
		return(-1);
	}
	
	switch(level)
	{
		case SOL_SOCKET:
			return __otter_libc_setsockopt_sol_socket(sock, option_name, option_value, option_len);
		case IPPROTO_IP:
		case IPPROTO_ICMP:
		case IPPROTO_TCP:
		case IPPROTO_UDP:
			/* TODO: impliment somthing that makes sense so these (non-existant) protocol layers */
		default:
			errno = EINVAL;
			return(-1);
	}
	
	return(-1);
}

int __otter_libc_getsockopt(int socket, int level, int option_name, void *option_value, socklen_t *option_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket);
	if(!sock)
	{
		return(-1);
	}
	
	switch(level)
	{
		case SOL_SOCKET:
			return __otter_libc_getsockopt_sol_socket(sock, option_name, option_value, option_len);
		case IPPROTO_IP:
		case IPPROTO_ICMP:
		case IPPROTO_TCP:
		case IPPROTO_UDP:
			/* TODO: impliment somthing that makes sense so these (non-existant) protocol layers */
		default:
			errno = EINVAL;
			return(-1);
	}
	
	return(-1);
}

int bind(int socket, const struct sockaddr *address, socklen_t address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket);
	if(!sock)
	{
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

	/* look for a conflict with the address (already assigned and not SO_REUSEADDR) */
	if(__otter_array_fold(
			/*begin fun (prev:*/int,/*) (cur:ref */int,/*) ->*/
			({
				struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket);
				if(sock == NULL) /* this was not a socket */
					prev;
				if(sock->addr == NULL) /* sock is not bound */
					prev;
				if(memcmp(sock->addr, address, address_len) == 0)
				{ /* a matching address was found, check to see that reuse is allowed */
					int l = 1;
					getsockopt(*cur, SOL_SOCKET, SO_REUSEADDR, &l, sizeof(int));
					if(!l)
						1;
					__otter_libc_getsockopt_sol_socket(sock, SO_REUSEADDR, &l, sizeof(int));
					if(!l)
						1;
				}
				0;
			}),
			/*end*/
			__otter_fs_fd_table,
			__otter_fs_MAXOPEN,
			0
		))
	{
		errno = EADDRINUSE;
		return(-1);
	}
	
	memcpy(sock->addr, address, address_len);
	l = 1;
	__otter_libc_setsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	
	return(0);
}

int listen(int socket, int backlog)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket);
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
	
	return(0);
	
}
