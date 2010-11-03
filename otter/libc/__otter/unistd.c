#include<__otter/otter_fs.h>
#include<stdlib.h>
#include<string.h>
#include<fcntl.h>
#include<__otter/otter_user.h>
#include <sys/socket.h>
#include <errno.h>

int __otter_libc_close(int fd)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	__otter_fs_fd_table[fd] = -1;

	open_file->openno--;

	if(open_file->type == __otter_fs_TYP_DIR)
	{
		struct __otter_fs_dnode* dnode = (struct __otter_fs_dnode*)open_file->vnode;
		(*dnode).r_openno--;

		if((*dnode).linkno == 0 && (*dnode).r_openno == 0) /* directory was deleted, but left available until closed */
		{
			free((*dnode).dirs);
			free((*dnode).files);
			free(dnode);
		}

		return (0);
	}

	if(open_file->openno == 0) /* open file table entry is no longer in use */
	{
		struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
		if(open_file->mode & O_RDONLY)
			(*inode).r_openno--;
		if(open_file->mode & O_WRONLY)
			(*inode).w_openno--;

		if((*inode).r_openno | (*inode).w_openno) /* file is still open */
			return (0);
		
		if(open_file->type == __otter_fs_TYP_SOCK) /* shutdown socket */
		{
			struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)((struct __otter_fs_inode*)(open_file->vnode))->data;
			if(!sock)
			{
				return(-1);
			}
		
			shutdown(fd, SHUT_RDWR);
			switch(sock->state)
			{
				case __otter_sock_ST_CLOSED:
					break;
				case __otter_sock_ST_LISTEN:
				case __otter_sock_ST_SYN_RCVD:
					__otter_libc_flush_sock_queue(sock);
					break;
				case __otter_sock_ST_SYN_SENT:
					{
						int	found = 0;
						for(int i = 0; i < sock->sock_queue[0]->backlog; i++)
						{
							if(sock->sock_queue[0]->sock_queue[i] == sock)
							{
								found = 1;
							}
							
							if(found)
							{
								sock->sock_queue[0]->sock_queue[i] = sock->sock_queue[0]->sock_queue[i + 1];
							}
						}
						
						if(found)
							sock->sock_queue[0]->sock_queue[sock->sock_queue[0]->backlog + 1] = NULL;
						else if(sock->sock_queue[0]->sock_queue[sock->sock_queue[0]->backlog + 1] == sock)
							sock->sock_queue[0]->sock_queue[sock->sock_queue[0]->backlog + 1] = NULL;
						else
							__ASSERT(0);
					}
					break;
				case __otter_sock_ST_ESTABLISHED:
				case __otter_sock_ST_CLOSE_WAIT:
					shutdown(fd, SHUT_RDWR);
					break;
				case __otter_sock_ST_LAST_ACK:
					break;
				case __otter_sock_ST_FIN_WAIT_1:
				case __otter_sock_ST_FIN_WAIT_2:
					shutdown(fd, SHUT_RDWR);
					break;
				case __otter_sock_ST_CLOSING:
				case __otter_sock_ST_TIME_WAIT:
				case __otter_sock_ST_UDP:
					break;
				default:
					__ASSERT(0);
			}
			
			__otter_fs_free_socket(inode);
			return(0);
		}

		if(open_file->type == __otter_fs_TYP_FIFO) /* discard data if this is a fifo */
		{
			(*inode).size = 0;
			free((*inode).data);
			(*inode).data = NULL;
		}
	
		if((*inode).linkno == 0) /* file was deleted, but left available until closed */
		{
			free((*inode).data);
			free(inode);	
		}
	}

	return (0);
}

/* linear buffer */
ssize_t __otter_libc_read_file(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)(open_file->vnode);

	for(int i = 0; i < num; i++)
	{
		if(i + offset < (*inode).size)
		{
			buf[i] = (*inode).data[i + offset];
		}
		else
		{
			open_file->status = __otter_fs_STATUS_EOF;
			return (i);
		}
	}

	return (num);
}

ssize_t __otter_libc_write_file(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)(open_file->vnode);

	int physicalsize = __otter_fs_BLOCK_SIZE * (*inode).numblocks; /* get the amount of allocated space */

	if(offset + num > physicalsize) /* there isn't enough space */
	{
		/* make more space 
		 * In the even that the new size would exactly fill a new block, allocating an extra one won't hurt;
		 * it will probably be needed.
		 */
		int newblocks = (((offset + num) / __otter_fs_BLOCK_SIZE) + 1);
		(*inode).data = (char*)__otter_multi_grealloc((*inode).data, newblocks * __otter_fs_BLOCK_SIZE);
		/* initilize new blocks to 0; if the write was past then end of the file, the gap should be set to 0 */
		memset((*inode).data + physicalsize, 0, (newblocks - (*inode).numblocks) * __otter_fs_BLOCK_SIZE);
		(*inode).numblocks = newblocks;
	}

	for(int i = 0; i < num; i++)
	{
		(*inode).data[i + offset] = ((char*)buf)[i];
	}

	if((*inode).size < offset + num)
	{
		(*inode).size = offset + num;
	}

	return (num);
}

/* generate symbolic data */
ssize_t __otter_libc_read_tty(void* buf, size_t num)
{
	char data;
	for(int i = 0; i < num; i++)
	{
		data = __SYMBOLIC(1);
		/* This is a device not a file redirected to stdin; use 0 to indicate crtl+D was pressed to stop input */
		if(data != 0)
		{
			buf[i] = data;
		}
		else /* it might be nice to make '\n' terminate lines, but that might not happen in general */
		{
			return (i);
		}
	}
	return (num);
}

/* circular buffer */
ssize_t __otter_libc_read_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num)
{
	__otter_multi_begin_atomic();
	for(int i = 0; i < num; i++)
	{
		/* next unread char is not the next char to be written to*/
		if((i + pipe->rhead + 1) % __otter_fs_PIPE_SIZE != pipe->whead)
		{
			buf[i] = pipe->data[(i + pipe->rhead + 1) % __otter_fs_PIPE_SIZE];
		}
		else /* no more new chars to read */
		{
			pipe->rhead = (i + pipe->rhead) % __otter_fs_PIPE_SIZE;
			__otter_multi_end_atomic();
			return (i);
		}
	}
	__otter_multi_end_atomic();

	pipe->rhead = (pipe->rhead + num) % __otter_fs_PIPE_SIZE; /* move rhead to last char read */
	return (num);
}

ssize_t __otter_libc_write_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num)
{
	__otter_multi_begin_atomic();
	for(int i = 0; i < num; i++)
	{
		/* current char is not the last char read */
		if((i + pipe->whead) % __otter_fs_PIPE_SIZE != pipe->rhead)
		{
			pipe->data[(i + pipe->whead) % __otter_fs_PIPE_SIZE] = ((char*)buf)[i];
		}
		else /* write head has run out of space; must block until some has be read */
		{
			while((i + pipe->whead) % __otter_fs_PIPE_SIZE != pipe->rhead)
			{
				__otter_multi_io_block(pipe->data);
				__otter_multi_begin_atomic();
			}
		}
	}
	__otter_multi_end_atomic();

	pipe->whead = (pipe->whead + num) % __otter_fs_PIPE_SIZE; /* move whead to next free char to write */
	return (num);
}

ssize_t __otter_libc_read_pipe(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_pipe_data* pipe = (struct __otter_fs_pipe_data*)inode->data;
	if(open_file->status == __otter_fs_STATUS_EOF)
	{
		if(open_file->mode & O_NONBLOCK)
		{
			errno = EAGAIN;
			return (-1);
		}

		/* block until data becomes available */
		__otter_multi_begin_atomic();
		while(open_file->status == __otter_fs_STATUS_EOF)
		{
			__otter_multi_io_block(open_file);
			__otter_multi_begin_atomic();
		}
		__otter_multi_end_atomic();

	}

	int num = __otter_libc_read_pipe_data(pipe, buf, num);
	if((pipe->rhead + 1) % __otter_fs_PIPE_SIZE == pipe->whead) /* set EOF if there are no more chars left to read */
	{
		open_file->status = __otter_fs_STATUS_EOF;
	}
	
	return(num);
}

ssize_t __otter_libc_write_pipe(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_pipe_data* pipe = (struct __otter_fs_pipe_data*)inode->data;
	if((*inode).r_openno == 0) /* not open for reading somewhere; must be open for reading before actually writing */
	{
		errno = EPIPE;
		/* raise(SIGPIPE); */
		return (-1);
	}
	
	return __otter_libc_write_pipe_data(pipe, buf, num);
}

/* double dircular buffer */
ssize_t __otter_libc_read_socket(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)inode->data;
	
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
			/* can't read() in these states */
			errno = ENOTCONN;
			return(-1);
			break;

		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
			return __otter_libc_read_pipe_data(sock->recv_data, buf, num);
			break;
			
		default:
			__ASSERT(0);
	}
	return(-1);
}

ssize_t __otter_libc_write_socket(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num)
{
	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	struct __otter_fs_sock_data* sock = (struct __otter_fs_sock_data*)inode->data;
	
	switch(sock->state)
	{
		case __otter_sock_ST_CLOSED:
		case __otter_sock_ST_LISTEN:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
		case __otter_sock_ST_UDP:
			/* can't write() in these states */
			errno = ENOTCONN;
			return(-1);
			break;

		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
			{
				return __otter_libc_write_pipe_data(sock->sock_queue[0]->recv_data, buf, num);
				break;
			}
			
		default:
			__ASSERT(0);
	}
	
	return(-1);
}

ssize_t __otter_libc_read2(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	if(num == 0)
		return(0);

	if(offset < 0)
	{
		errno = EINVAL;
		return (-1);
	}

	switch (open_file->type)
	{
		case __otter_fs_TYP_FILE:
			return __otter_libc_read_file(open_file, buf, num, offset);
			break;
		case __otter_fs_TYP_TTY:
			return __otter_libc_read_tty(buf, num);
			break;
		case __otter_fs_TYP_FIFO:
			return __otter_libc_read_pipe(open_file, buf, num);
			break;
		case __otter_fs_TYP_SOCK:
			return __otter_libc_read_socket(open_file, buf, num);
			break;
		case __otter_fs_TYP_DIR: /* reading from directories is not supported */
			errno = EISDIR;
			return (-1);
			break;
		case __otter_fs_TYP_NULL:
			open_file->status = __otter_fs_STATUS_EOF;
			return (0);
			break;
		case __otter_fs_TYP_ZERO:
			open_file->status = __otter_fs_STATUS_OK;
			for(int i = 0; i < num; i++)
			{
					buf[i] = 0;
			}

			return (num);

			break;
		default: /* this should never happen as all cases should be enumerated */
			__ASSERT(0);
			break;
	}

	return (0);
}

ssize_t __otter_libc_read(int fd, void* buf, size_t num)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	if(!(open_file->mode & O_RDONLY)) /* open for reading? */
	{
		errno = EBADF;
		return(-1);
	}

	int numread = __otter_libc_read2(open_file, buf, num, open_file->offset);

	if(numread == -1)
		return (-1);

	open_file->offset += numread;
	return (numread);
}

ssize_t __otter_libc_pread(int fd, void* buf, size_t num, off_t offset)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	if(!(open_file->mode & O_RDONLY)) /* open for reading? */
	{
		errno = EBADF;
		return(-1);
	}

	if(open_file->type != __otter_fs_TYP_FILE) /* can only seek on normal files */
	{
		errno = ESPIPE;
		return (-1);
	}

	return __otter_libc_read2(open_file, buf, num, offset);
}

ssize_t __otter_libc_write2(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	if(num == 0)
		return(0);

	if(offset < 0)
	{
		errno = EINVAL;
		return (-1);
	}

	switch (open_file->type)
	{
		case __otter_fs_TYP_FILE:
		case __otter_fs_TYP_TTY:
			return __otter_libc_write_file(open_file, buf, num, offset);
			break;
		case __otter_fs_TYP_FIFO:
			return __otter_libc_write_pipe(open_file, buf, num);
			break;
		case __otter_fs_TYP_SOCK:
			return __otter_libc_write_socket(open_file, buf, num);
			break;
		case __otter_fs_TYP_DIR: /* writing to directories is not supported */
			__ASSERT(0); /* should not be possible to open a dir for writing */
			break;
		case __otter_fs_TYP_NULL:
		case __otter_fs_TYP_ZERO:
			open_file->status = __otter_fs_STATUS_EOF;
			return (num);
			break;
		default: /* this should never happen as all cases should be enumerated */
			__ASSERT(0);
			break;
	}

	return (0);
}

ssize_t __otter_libc_write(int fd, const void* buf, size_t num)
{	
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	if(!(open_file->mode & O_WRONLY)) /* open for writing? */
	{
		errno = EBADF;
		return(-1);
	}

	if(open_file->mode & O_APPEND)
	{
		open_file->offset = (*((struct __otter_fs_inode*)open_file->vnode)).size;
	}

	int numwrite = __otter_libc_write2(open_file, buf, num, open_file->offset);

	if(numwrite == -1)
		return (-1);

	/* Is there a race condition here if two processes both have
		 references to the same __otter_fs_open_file? What is the offset
		 supposed to be while a write is happening? */
	open_file->offset += numwrite;
	return (numwrite);
}

ssize_t __otter_libc_pwrite(int fd, const void* buf, size_t num, off_t offset)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	if(!(open_file->mode & O_WRONLY)) /* open for writing? */
	{
		errno = EBADF;
		return(-1);
	}

	if(open_file->type != __otter_fs_TYP_FILE) /* can only seek on normal files */
	{
		errno = ESPIPE;
		return (-1);
	}

	return __otter_libc_write2(open_file, buf, num, offset);
}

int __otter_libc_unlink(const char* path)
{
	char *filename;
	struct __otter_fs_dnode* dnode = find_filename_and_dnode(path, &filename);

	if(!dnode) /* can't find path */
		return (-1);

	return (__otter_fs_unlink_in_dir(filename, dnode) - 1);

}

int __otter_libc_rmdir(const char* path)
{
	char *filename;
	struct __otter_fs_dnode* dnode = find_filename_and_dnode(path, &filename);

	if(!dnode) /* can't find path */
		return (-1);

	return (__otter_fs_rmdir_in_dir(filename, dnode) - 1);
}

off_t __otter_libc_lseek(int fd, off_t offset, int whence)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;
	
	if(open_file->type != __otter_fs_TYP_FILE) /* can only seek on normal files */
	{
		errno = ESPIPE;
		return (-1);
	}
	
	off_t newOffset;
	
	switch (whence)
	{
		case SEEK_SET:
			newOffset = offset;
			break;
		case SEEK_CUR:
			newOffset = open_file->offset + offset;
			break;
		case SEEK_END:
		{
			struct __otter_fs_inode* inode = open_file->vnode;
			newOffset = inode->size + offset;
			break;
		}
		default:
			errno = EINVAL;
			return -1;
	}
	if (newOffset < 0) {
		errno = EINVAL;
		return -1;
	}
	open_file->offset = newOffset;
	
	return newOffset;
}

uid_t __otter_libc_getuid(void)
{
	return(__otter_uid);
}

int __otter_libc_setuid(uid_t uid)
{
	if(uid == __otter_UID_ROOT || uid == __otter_UID_USER) /* only two valid users */
	{
		/*Assume that the user would authenticate as needed*/

		if(__otter_uid != uid) /*uid changed*/
		{
			__otter_fs_umask = __otter_fs_umask ^ 0x3000;  /* flip owner and user bits */
			__otter_uid = uid;

		}

		return(0);
	}

	errno = EINVAL;
	return(-1);
}

int __otter_libc_dup(int fd)
{
	return fcntl(fd, F_DUPFD, 0);
}

int __otter_libc_dup2(int fd1, int fd2)
{
	if(fd2 < 0 || fd2 >= __otter_fs_MAXOPEN)
	{
		errno = EBADF;
		return(-1);
	}

	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd1);
	if(!open_file)
	{
		errno = EBADF;
		return(-1);
	}

	if(__otter_fs_fd_table[fd1] == __otter_fs_fd_table[fd2])
		return(fd2);

	open_file = get_open_file_from_fd(fd2);
	
	if(open_file) /* is file a valid file entry? */
	{
		close(fd2);
	}

	return fcntl(fd1, F_DUPFD, fd2);
}

int __otter_libc_getpagesize()
{
	return((int)sysconf(_SC_PAGE_SIZE));
}

long __otter_libc_sysconf(int name)
{
	switch(name)
	{
		case _SC_PAGE_SIZE: /* _SC_PAGESIZE as well */
			return(4096);
		/* TODO: Impliment other system constants */
	}
	
	errno = EINVAL;
	return(-1);
}

int fork()
{
	__otter_multi_begin_atomic();
	struct __otter_fs_open_file_table_entry* open_file;

	if(__otter_fs_open_file_table)
	{
		for(int i = 0; i < __otter_fs_MAXOPEN; i++)
		{
			open_file = get_open_file_from_fd(i);
			if(open_file)
			{
				open_file->openno++;
			}
		}
	}
	
	__otter_multi_end_atomic();
	
	return __otter_multi_fork();
}
