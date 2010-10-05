#include<__otter/otter_fs.h>
#include<stdlib.h>
#include<string.h>
#include<fcntl.h>
#include<__otter/otter_user.h>

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

	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	if(open_file->mode & O_RDONLY)
		(*inode).r_openno--;
	if(open_file->mode & O_WRONLY)
		(*inode).w_openno--;

	if((*inode).r_openno | (*inode).w_openno) /* file is still open */
		return (0);

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

	return (0);
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
		/* linear buffer */
		case __otter_fs_TYP_FILE:
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
			break;
		}
		/* generate symbolic data */
		case __otter_fs_TYP_TTY:
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
			break;
		}
		/* circular buffer */
		case __otter_fs_TYP_FIFO:
		{
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
			if(open_file->status == __otter_fs_STATUS_EOF)
			{
				if(open_file->mode & O_NONBLOCK)
				{
					errno = EAGAIN;
					return (-1);
				}

				/* block until data becomes available */
				while(open_file->status == __otter_fs_STATUS_EOF)
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
					open_file->status = __otter_fs_STATUS_EOF;
					(*inode).numblocks = ((*inode).numblocks + i) % __otter_fs_PIPE_SIZE;
					return (i);
				}
			}

			(*inode).numblocks = ((*inode).numblocks + num) % __otter_fs_PIPE_SIZE;
			return (num);
			break;
		}
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
		/* linear buffer */
		case __otter_fs_TYP_FILE:
		case __otter_fs_TYP_TTY:
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
		}
		/* circular buffer */
		case __otter_fs_TYP_FIFO:
		{
			struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
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
		}
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

		if(__otter_uid ^ uid) /*uid changed*/
		{
			__otter_fs_umask = __otter_fs_umask ^ 0x3000;  /* flip owner and user bits */
			__otter_uid = uid;

		}

		return(0);
	}

	errno = EINVAL;
	return(-1);
}
