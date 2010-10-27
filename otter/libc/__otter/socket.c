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

int __otter_libc_setsockopt(int socket_fd, int level, int option_name, const void *option_value, socklen_t option_len)
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

int __otter_libc_getsockopt(int socket_fd, int level, int option_name, void *option_value, socklen_t *option_len)
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

int __otter_libc_bind(int socket_fd, const struct sockaddr *address, socklen_t address_len)
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
					a->sin_addr.s_addr = 0x7F000001; /* 127.0.0.1 */
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
					a->sin6_addr.s6_addr32[3] == 1; /* 0000:0000:0000:0000:0000:0000:0000:0001 */
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
	
	for(int i = 0; i < __otter_fs_MAXOPEN; i++)
	{
		struct __otter_fs_sock_data* other = __otter_libc_get_sock_data(__otter_fs_fd_table[i]);
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

	if(addr_used)
	{
		errno = EADDRINUSE;
		return(-1);
	}
	
	l = 1;
	__otter_libc_setsockopt_sol_socket(sock, SO_BOUND, &l, sizeof(int));
	
	return(0);
}

int __otter_libc_listen(int socket_fd, int backlog)
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

int __otter_libc_accept(int socket_fd, struct sockaddr *address, socklen_t *address_len)
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
	while(sock->state = __otter_sock_ST_LISTEN)
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
			sock_other->state = __otter_sock_ST_ESTABLISHED;
			
			return(fd);
		}
	}
	
	return(-1);
}

void __otter_libc_flush_sock_queue(struct __otter_fs_sock_data* sock)
{
	for(int i = 0; i < sock->backlog; i++)
	{
		if(sock->sock_queue[i] != NULL) /* there is something waiting here */
		{
			sock->sock_queue[i]->state = __otter_sock_ST_CLOSED;
			__otter_multi_gfree(sock->sock_queue[i]->sock_queue);
			sock->sock_queue[i]->sock_queue = NULL;
		}
	}
}

int __otter_libc_connect(int socket_fd, const struct sockaddr *address, socklen_t address_len)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data(socket_fd);
	if(!sock)
	{
		return(-1);
	}
	
	int	l = 0;
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
	
	/* check that the socket is in an appropriate state for accept() */
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
	
	int q_size = 0;
	struct __otter_fs_sock_data* best_sock = NULL;
	for(int i = 0; i < __otter_fs_GLOBALMAXOPEN; i++)
	{
		/* look for listening sockets */
		if(__otter_fs_open_file_table[i].type != __otter_fs_TYP_SOCK)
			continue;
		struct __otter_fs_sock_data* recv = (struct __otter_fs_sock_data*)__otter_fs_open_file_table[i].vnode;
		if(recv->state != __otter_sock_ST_LISTEN)
			continue;
		
		/* check that the addresses match */
		if(memcmp(recv->addr, sock->addr) == 0)
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
			return(0);
		}
	}
	
	errno = ECONNREFUSED;
	return(-1);
}

int __otter_libc_shutdown(int socket_fd, int how)
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
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
			errno = ENOTCONN;
			retunr(-1);
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
						return(0);
					default:
						errno = EINVAL;
						retunr(-1);
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
		case __otter_sock_ST_UDP: /* UDP dosn't listen */
			errno = EOPNOTSUPP;
			return(-1);
		default:
			__ASSERT(0);
	}

	__ASSERT(0);	
	return (-1);
}
