#include<__otter/otter_fs.h>
#include<stdlib.h>
#include<string.h>
#include<fcntl.h>

int __otter_libc_close(int fd)
{
	if(fd < 0 || fd >= __otter_fs_MAXOPEN)
	{
		errno = EBADF;
		return(-1);
	}
	
	int ft = __otter_fs_files[fd];
	__otter_fs_files[fd] = -1;

	if(ft == -1) /* is file a valid file entry? */
	{
		errno = EBADF;
		return(-1);
	}

	__otter_fs_open_files[ft].openno--;

	if(__otter_fs_open_files[ft].type == __otter_fs_TYP_DIR)
	{
		struct __otter_fs_dnode* dnode = __otter_fs_get_dnode_from_fd(fd);
		(*dnode).r_openno--;

		if((*dnode).linkno == 0 && (*dnode).r_openno == 0) /* directory was deleted, but left available until closed */
		{
			free((*dnode).dirs);
			free((*dnode).files);
			free(dnode);
		}

		return (0);
	}

	struct __otter_fs_inode* inode = __otter_fs_get_inode_from_fd(fd);
	if(__otter_fs_open_files[ft].mode & O_RDONLY)
		(*inode).r_openno--;
	if(__otter_fs_open_files[ft].mode & O_WRONLY)
		(*inode).w_openno--;

	if((*inode).r_openno | (*inode).w_openno) /* file is still open */
		return (0);

	if(__otter_fs_open_files[ft].type == __otter_fs_TYP_FIFO) /* discard data if this is a fifo */
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

	return (0);
}

ssize_t __otter_libc_read2(int ft, void* buf, size_t num, off_t offset)
{
	if(num == 0)
		return(0);

	if(offset < 0)
	{
		errno = EINVAL;
		return (-1);
	}

	switch (__otter_fs_open_files[ft].type)
	{
		/* linear buffer */
		case __otter_fs_TYP_FILE:;
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)(__otter_fs_open_files[ft].vnode);

			for(int i = 0; i < num; i++)
			{
				if(i + offset < (*inode).size)
				{
					buf[i] = (*inode).data[i + offset];
				}
				else
				{
					__otter_fs_open_files[ft].status = __otter_fs_STATUS_EOF;
					return (i);
				}
			}

			return (num);
			break;

		/* generate symbolic data */
		case __otter_fs_TYP_TTY:;
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
			break;

		/* circular buffer */
		case __otter_fs_TYP_FIFO:;
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)__otter_fs_open_files[ft].vnode;
			if(__otter_fs_open_files[ft].status == __otter_fs_STATUS_EOF)
			{
				if(__otter_fs_open_files[ft].mode & O_NONBLOCK)
				{
					errno = EAGAIN;
					return (-1);
				}

				/* block until data becomes available */
				while(__otter_fs_open_files[ft].status == __otter_fs_STATUS_EOF)
				{
					__ASSERT(0); /* TODO: if this is multiOtter, don't fail here */
				}
				
			}

			offset = (*inode).numblocks;

			for(int i = 0; i < num; i++)
			{
				if((i + offset) % __otter_fs_PIPE_SIZE != (*inode).size)
				{
					buf[i] = (*inode).data[(i + offset) % __otter_fs_PIPE_SIZE];
				}
				else
				{
					__otter_fs_open_files[ft].status = __otter_fs_STATUS_EOF;
					(*inode).numblocks = ((*inode).numblocks + i) % __otter_fs_PIPE_SIZE;
					return (i);
				}
			}

			(*inode).numblocks = ((*inode).numblocks + num) % __otter_fs_PIPE_SIZE;
			return (num);
			break;

		case __otter_fs_TYP_DIR: /* reading from directories is not supported */
			errno = EISDIR;
			return (-1);
			break;
		case __otter_fs_TYP_NULL:
			__otter_fs_open_files[ft].status = __otter_fs_STATUS_EOF;
			return (0);
			break;
		case __otter_fs_TYP_ZERO:
			__otter_fs_open_files[ft].status = __otter_fs_STATUS_OK;
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
	if(fd > -1 && fd < __otter_fs_MAXOPEN) /* is file a possible valid file? */
	{
		int ft = __otter_fs_files[fd];

		if(ft != -1) /* is file a valid file entry? */
		{

			if(!(__otter_fs_open_files[ft].mode & O_RDONLY)) /* open for reading? */
			{
				errno = EBADF;
				return(-1);
			}

			int numread = __otter_libc_read2(fd, buf, num, __otter_fs_open_files[ft].offset);

			if(numread == -1)
				return (-1);

			__otter_fs_open_files[ft].offset += numread;
			return (numread);
		}
	}

	errno = EBADF;
	return(-1);
}

ssize_t __otter_libc_pread(int fd, void* buf, size_t num, off_t offset)
{
	if(fd < 0 || fd >= __otter_fs_MAXOPEN)
	{
		errno = EBADF;
		return(-1);
	}

	int ft = __otter_fs_files[fd];

	if(ft == -1) /* is file a valid file entry? */
	{
		errno = EBADF;
		return(-1);
	}

	if(!(__otter_fs_open_files[ft].mode & O_RDONLY)) /* open for reading? */
	{
		errno = EBADF;
		return(-1);
	}

	if(__otter_fs_open_files[ft].type != __otter_fs_TYP_FILE) /* can only seek on normal files */
	{
		errno = ESPIPE;
		return (-1);
	}

	return __otter_libc_read2(ft, buf, num, offset);
}

ssize_t __otter_libc_write2(int ft, void* buf, size_t num, off_t offset)
{
	if(num == 0)
		return(0);

	if(offset < 0)
	{
		errno = EINVAL;
		return (-1);
	}

	switch (__otter_fs_open_files[ft].type)
	{
		/* linear buffer */
		case __otter_fs_TYP_FILE:
		case __otter_fs_TYP_TTY:;
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)(__otter_fs_open_files[ft].vnode);

			int physicalsize = __otter_fs_BLOCK_SIZE * (*inode).numblocks; /* get the amount of allocated space */

			if(offset + num > physicalsize) /* there isn't enough space */
			{
				/* make more space 
				 * In the even that the new size would exactly fill a new block, allocating an extra one won't hurt;
				 * it will probably be needed.
				 */
				int newblocks = (((offset + num) / __otter_fs_BLOCK_SIZE) + 1);
				(*inode).data = (char*)realloc((*inode).data, newblocks * __otter_fs_BLOCK_SIZE);
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
			break;

		/* circular buffer */
		case __otter_fs_TYP_FIFO:;
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)__otter_fs_open_files[ft].vnode;
			if((*inode).r_openno == 0) /* not open for reading somewhere */
			{
				errno = EPIPE;
				/* raise(SIGPIPE); */
				return (-1);
			}

			offset = (*inode).size;

			for(int i = 0; i < num; i++)
			{
				if((i + offset) % __otter_fs_PIPE_SIZE != (*inode).numblocks)
				{
					buf[i] = (*inode).data[(i + offset) % __otter_fs_PIPE_SIZE];
				}
				else /* write head has run out of space; must block until some has be read */
				{
					__ASSERT(0); /* TODO: make the work right for multiotter */
				}
			}

			(*inode).size = ((*inode).size + num) % __otter_fs_PIPE_SIZE;
			return (num);
			break;

		case __otter_fs_TYP_DIR: /* writing to directories is not supported */
			__ASSERT(0); /* should not be possible to open a dir for writing */
			break;
		case __otter_fs_TYP_NULL:
		case __otter_fs_TYP_ZERO:
			__otter_fs_open_files[ft].status = __otter_fs_STATUS_EOF;
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
	if(fd > -1 && fd < __otter_fs_MAXOPEN) /* is file a possible valid file? */
	{
		int ft = __otter_fs_files[fd];

		if(ft != -1) /* is file a valid file entry? */
		{

			if(!(__otter_fs_open_files[ft].mode & O_WRONLY)) /* open for writing? */
			{
				errno = EBADF;
				return(-1);
			}

			if(__otter_fs_open_files[ft].mode & O_APPEND)
			{
				__otter_fs_open_files[ft].offset = (*((struct __otter_fs_inode*)__otter_fs_open_files[ft].vnode)).size;
			}

			int numwrite = __otter_libc_write2(fd, buf, num, __otter_fs_open_files[ft].offset);

			if(numwrite == -1)
				return (-1);

			__otter_fs_open_files[ft].offset += numwrite;
			return (numwrite);
		}
	}

	errno = EBADF;
	return(-1);
}

ssize_t __otter_libc_pwrite(int fd, const void* buf, size_t num, off_t offset)
{
	if(fd < 0 || fd >= __otter_fs_MAXOPEN)
	{
		errno = EBADF;
		return(-1);
	}

	int ft = __otter_fs_files[fd];

	if(ft == -1) /* is file a valid file entry? */
	{
		errno = EBADF;
		return(-1);
	}

	if(!(__otter_fs_open_files[ft].mode & O_WRONLY)) /* open for writing? */
	{
		errno = EBADF;
		return(-1);
	}

	if(__otter_fs_open_files[ft].type != __otter_fs_TYP_FILE) /* can only seek on normal files */
	{
		errno = ESPIPE;
		return (-1);
	}

	return __otter_libc_write2(ft, buf, num, offset);
}

int __otter_libc_unlink(const char* path)
{
	char* a = path;

	while(*a != 0)
		a++;

	while(a != path && (*a) != '/')
		a--;
	
	struct __otter_fs_dnode* dnode;
	if(a == path)
	{
		dnode = __otter_fs_pwd;
	}
	else
	{
		*a = 0;
		dnode = __otter_fs_find_dnode(path);
		*a = '/';
	}

	if(!dnode) /* can't find path */
		return (-1);

	a++;

	return (__otter_fs_unlink_in_dir(a, dnode) - 1);

}

int __otter_libc_rmdir(const char* path)
{
	char* a = path;

	while(*a != 0)
		a++;

	while(a != path && (*a) != '/')
		a--;
	
	struct __otter_fs_dnode* dnode;
	if(a == path)
	{
		dnode = __otter_fs_pwd;
	}
	else
	{
		*a = 0;
		dnode = __otter_fs_find_dnode(path);
		*a = '/';
	}

	if(!dnode) /* can't find path */
		return (-1);

	a++;

	return (__otter_fs_rmdir_in_dir(a, dnode) - 1);
}
