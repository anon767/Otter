#include "otter/otter_fs.h"
#include "otter/otter_builtins.h"
#include "otter/otter_user.h"
#include "otter/otter_scheduler.h"
#include "otter/multiotter_builtins.h"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <errno.h>

int __otter_libc_close(int fd)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	__otter_multi_begin_atomic();
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

		__otter_multi_end_atomic();
		return (0);
	}

	if(open_file->openno == 0) /* open file table entry is no longer in use */
	{
		struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
		if(open_file->mode & O_RDONLY)
			(*inode).r_openno--;
		if(open_file->mode & O_WRONLY)
			(*inode).w_openno--;

		if((*inode).r_openno || (*inode).w_openno) /* file is still open */
		{
			__otter_multi_end_atomic();
			return (0);
		}
		
		if(open_file->type == __otter_fs_TYP_SOCK) /* shutdown socket */
		{
			struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data_from_open_file(open_file);
			
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
						
						sock->sock_queue[0] = NULL;
					}
					break;
				case __otter_sock_ST_ESTABLISHED:
				case __otter_sock_ST_CLOSE_WAIT:
					__otter_libc_shutdown_sock_data(sock, SHUT_RDWR);
					break;
				case __otter_sock_ST_LAST_ACK:
					__ASSERT(0);
					break;
				case __otter_sock_ST_FIN_WAIT_1:
				case __otter_sock_ST_FIN_WAIT_2:
					__otter_libc_shutdown_sock_data(sock, SHUT_RDWR);
					break;
				case __otter_sock_ST_CLOSING:
				case __otter_sock_ST_TIME_WAIT:
					__ASSERT(0);
				case __otter_sock_ST_UDP:
					break;
				default:
					__ASSERT(0);
			}
			
			__otter_fs_free_socket(inode);
			__otter_multi_end_atomic();
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
	
	__otter_multi_end_atomic();
	return (0);
}

/* linear buffer */
ssize_t __otter_libc_read_file(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	__otter_multi_begin_atomic();

	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)(open_file->vnode);

	/* Copy into buf the bytes between inode->data[offset] and
		inode->data[offset+num-1], but don't read beyond
		inode->data[inode->size - 1]. That means that if inode->size <
		offset+num, we only read inode->size - offset bytes instead of
		num. */
	num = inode->size < offset + num ? inode->size - offset : num;
	memcpy(buf, &inode->data[offset], num);
	
	__otter_multi_end_atomic();

	return (num);
}

ssize_t __otter_libc_write_file(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num,
	off_t offset)
{
	__otter_multi_begin_atomic();
	
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

	memcpy(inode->data + offset, buf, num);

	if((*inode).size < offset + num)
	{
		(*inode).size = offset + num;
	}
	
	__otter_multi_end_atomic();

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
			((char *)buf)[i] = data;
		}
		else /* it might be nice to make '\n' terminate lines, but that might not happen in general */
		{
			return (i);
		}
	}
	return (num);
}

/* circular buffer */
ssize_t __otter_libc_pread_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num)
{
	__otter_multi_begin_atomic();
	
	// TODO: we can replace this loop with a length check and one or two calls to memcpy
	for(int i = 0; i < num; i++)
	{
		/* next unread char is not the next char to be written to*/
		if((i + pipe->rhead + 1) % __otter_fs_PIPE_SIZE != pipe->whead)
		{
			((char *)buf)[i] = pipe->data[(i + pipe->rhead + 1) % __otter_fs_PIPE_SIZE];
		}
		else /* no more new chars to read */
		{
			__otter_multi_end_atomic();
			return (i);
		}
	}
	__otter_multi_end_atomic();

	return (num);
}

/* Reads up to num bytes from pipe into buf, blocking if pipe is empty and nonblocking is false */
ssize_t __otter_libc_read_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num)
{
	num = __otter_libc_pread_pipe_data(pipe, buf, num);

	pipe->rhead = (pipe->rhead + num) % __otter_fs_PIPE_SIZE; /* move rhead to last char read */
	return num;
}

ssize_t __otter_libc_write_pipe_data(
	struct __otter_fs_pipe_data* pipe,
	void* buf,
	size_t num)
{
	__otter_multi_begin_atomic();
	// TODO: we should be able to replace this loop with a length check and one or two calls to memcpy
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
				__otter_multi_io_block(&pipe->rhead);
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
	struct __otter_fs_pipe_data* pipe = __otter_libc_get_pipe_data_from_open_file(open_file);
	
	/* Unless O_NONBLOCK is set, block until data becomes available. Then read the data. */
	if (__otter_fs_pipe_is_empty(pipe)) {
		if(open_file->mode & O_NONBLOCK)
		{
			errno = EAGAIN;
			return -1;
		}
		__otter_multi_block_while_condition(__otter_fs_pipe_is_empty(pipe), pipe);
	}
	/* This is a TOCTTOU problem on whether the pipe is empty, but opengroup says:
		'The behavior of multiple concurrent reads on the same pipe, FIFO, or terminal device is unspecified.'
		so this will (hopefully) never cause trouble. */

	return __otter_libc_read_pipe_data(pipe, buf, num);
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

ssize_t __otter_libc_write_socket(
	struct __otter_fs_open_file_table_entry* open_file,
	void* buf,
	size_t num)
{
	struct __otter_fs_sock_data* sock = __otter_libc_get_sock_data_from_open_file(open_file);
	
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

		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_CLOSE_WAIT:
			{
				return __otter_libc_write_pipe_data(sock->sock_queue[0]->recv_data, buf, num);
			}
	}
	__ASSERT(0);
	abort();
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
		case __otter_fs_TYP_TTY:
			return __otter_libc_read_tty(buf, num);
		case __otter_fs_TYP_FIFO:
			return __otter_libc_read_pipe(open_file, buf, num);
		case __otter_fs_TYP_SOCK:
			/* 'If fildes refers to a socket, read() shall be equivalent to recv() with no flags set.' */
			return __otter_libc_recv_socket(open_file, buf, num, 0);
		case __otter_fs_TYP_DIR: /* reading from directories is not supported */
			errno = EISDIR;
			return (-1);
		case __otter_fs_TYP_NULL:
			return (0);
		case __otter_fs_TYP_ZERO:
			memset(buf, 0, num);
			return (num);
	}
	__ASSERT(0); /* this should never happen as all cases should be enumerated */
	abort();
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
		case __otter_fs_TYP_TTY:
			__EVALSTR(buf, num);
			/* Do we want to fall through here and
				actually write to the TTY? I don't see why
				we would, because you can never read the
				data back in. */
			return num;
		case __otter_fs_TYP_FILE:
			return __otter_libc_write_file(open_file, buf, num, offset);
		case __otter_fs_TYP_FIFO:
			return __otter_libc_write_pipe(open_file, buf, num);
		case __otter_fs_TYP_SOCK:
			return __otter_libc_write_socket(open_file, buf, num);
		case __otter_fs_TYP_DIR: /* writing to directories is not supported */
			__ASSERT(0); /* should not be possible to open a dir for writing */
			abort();
		case __otter_fs_TYP_NULL:
		case __otter_fs_TYP_ZERO:
			return (num);
	}
	__ASSERT(0); /* this should never happen as all cases should be enumerated */
	abort();
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

gid_t __otter_libc_getgid()
{
	return __otter_gid;
}

int __otter_libc_setgid(gid_t gid)
{
	if(gid == __otter_GID_ROOT || gid == __otter_GID_USER) /* only two valid groups */
	{
		/*Assume that the user would authenticate as needed*/

		__otter_gid = gid;

		return(0);
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

pid_t __otter_libc_setsid()
{
	pid_t parent = getppid();
	if(parent < 0)
	{
		errno = EPERM;
		return(-1);
	}
	
	pid_t pid = getpid();
	__otter_multi_set_parent_pid(-pid - 2);
	return(-pid - 2);
}

pid_t __otter_libc_getppid()
{
	return __otter_multi_get_parent_pid(getpid());
}

gid_t __otter_libc_getegid()
{
	return getgid();
}

uid_t __otter_libc_geteuid()
{
	return getuid();
}

pid_t __otter_libc_getpgid(pid_t pid)
{
	int ppid = pid;
	while(ppid >= 0)
	{
		pid = ppid;
		ppid = __otter_multi_get_parent_pid(pid);
	}
	
	return pid;
}

pid_t __otter_libc_getpgrp()
{
	return getpgid(getpid());
}

int getgroups(int size, gid_t* list)
{
	/* supplimental groups are not implimented so there are none */
	return(0);
}

int chdir(const char *path)
{
	char* name = malloc(strlen(path) + 1);
	strcpy(name, path);
	struct __otter_fs_dnode* dnode = __otter_fs_find_dnode(name);
	
	if(!dnode)
	{
		free(name);
		return(-1);
	}
	
	__otter_fs_pwd = dnode;
	
	free(name);
	return(0);
}

int fchdir(int fd)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if(!open_file)
	{
		return(-1);
	}
	
	if(open_file->type != __otter_fs_TYP_DIR)
	{
		errno = ENOTDIR;
		return(-1);
	}
	
	__otter_fs_pwd = (struct __otter_fs_dnode*)open_file->vnode;
	return(0);
}

unsigned int alarm(unsigned int seconds)
{
	/* signals aren't implimented */
	return(0);
}

char *getcwd(char *buf, size_t len)
{
	strcpy(buf, "/wrk");
	return buf;
}
