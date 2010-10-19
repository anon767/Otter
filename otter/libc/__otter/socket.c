#include <sys/socket.h>
#include <__otter/otter_fs.h>
#include <fcntl.h>
#include <stdlib.h>
#include <netinet/in.h>

int __otter_libc_socket(int domain, int type, int protocol)
{
	int fd = -1;

	switch(domain)
	{
		case AF_UNIX:
		case AF_INET:
			{
				struct __otter_fs_inode* inode = __otter_fs_init_new_socket();
				
				if(type == SOCK_STREAM) /* UDP sockets are stateless */
				{
					((struct __otter_fs_sock_data*)inode->data)->state = __otter_sock_ST_UDP;
				}
				
				fd = __otter_fs_open_file(inode, O_RDWR);
				
				if(fd == -1) /* open failed */
				{
					__otter_libc_free_socket(inode);
				}
				
				break;
			}
			
		case AF_UNSPEC:
		default:
			errno = EAFNOSUPPORT;
			break;
	}
	
	return(fd);
}

int __otter_libc_setsockopt_sol_socket(struct __otter_fs_sock_data* sock, int option_name, const void* option_value, socklen_t option_len)
{
	switch(option_name)
	{
		/* TODO: impliment more of these */
		case SO_REUSEADDR:
			if(option_len < sizeof(int))
			{
				errno = EFAULT;
				return(-1);
			}
			
			if(*((int*)option_value))
			{
				sock->options |= SO_REUSEADDR;
			}
			else
			{
				sock->options &= ~SO_REUSEADDR;
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
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(socket);
	if(!open_file)
	{
		return(-1);
	}
	
	if(open_file->type != __otter_fs_TYP_SOCK)
	{
		errno = ENOTSOCK;
		retunr(-1);
	}
	
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)inode->data;
	
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
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(socket);
	if(!open_file)
	{
		return(-1);
	}
	
	if(open_file->type != __otter_fs_TYP_SOCK)
	{
		errno = ENOTSOCK;
		retunr(-1);
	}
	
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)inode->data;
	
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
