#include <sys/socket.h>
#include <__otter/otter_fs.h>
#include <fcntl.h>
#include <stdlib.h>

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
