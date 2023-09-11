#include "otter/otter_fs.h"
#include "otter/otter_builtins.h"

#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

int creat(const char* path, mode_t mode)
{
	return open(path, O_WRONLY | O_CREAT | O_TRUNC, mode);
}

/* TODO: when the functions this should be calling are done finish this one */
int fcntl(int fd, int cmd, ...)
{
	va_list varargs;
	va_start(varargs, cmd);

	int r = -1;

	switch (cmd)
	{
		case F_DUPFD:
			{
				int start = va_arg(varargs, int);
				int fd2 = __otter_fs_more_fd(start);
				if(fd2 == -1)
				{
					errno = EMFILE;
					break;
				}

				struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
				if(open_file) /* is file a valid file entry? */
				{
					__otter_fs_fd_table[fd2] = __otter_fs_fd_table[fd];
					open_file->openno++;
					r = fd2;
				}
				break;
			}
		case F_GETFD: 
			break;
		case F_SETFD: 
			break;
		case F_GETFL:
		{
			struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
			if(open_file) /* is file a valid file entry? */
			{
				r = open_file->mode;
			}
			break;
		}
		case F_SETFL:
		{
			int mode = va_arg(varargs, int);

			struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);

			if(open_file) /* is file a valid file entry? */
			{
				r = __otter_fs_change_open_mode(open_file, mode);
			}
			break;
		}
		case F_GETLK: 
			break;
		case F_SETLK: 
			break;
		case F_SETLKW: 
			break;
		case F_GETOWN: 
			break;
		case F_SETOWN: 
			break;
		default:
			errno = EINVAL;
			break;
	}

	va_end(varargs);

	return r;
}

int __otter_libc_vopen(const char* path, int oflag, va_list varargs)
{
	if(O_NONBLOCK & oflag) /* non blocking I/O not supported */
	{
		/*__ASSERT(0);*/
		/* ignore nonblock */
	}

	if (O_TRUNC & oflag && !(O_WRONLY & oflag)) /* undefined behavior */
	{
		__ASSERT(0);
	}

	struct __otter_fs_dnode* dnode = __otter_fs_find_dnode(path);
	if(dnode) /* is path a directory that already exists? */
	{
		if (O_TRUNC & oflag) /* undefined behavior */
		{
			__ASSERT(0);
		}

		return __otter_fs_open_dir(dnode, oflag);
	}

	/* we are looking for a file */
	
	char *filename;
	dnode = find_filename_and_dnode(path, &filename);

	if(!dnode) /* can't find path */
	{
		errno = ENOENT;
		return (-1);
	}

	struct __otter_fs_inode* inode = __otter_fs_find_inode_in_dir(filename, dnode);
	if(!inode) /* file not found or need permission to browse dir */
	{
		if(O_CREAT & oflag) /* should the file be created? */
		{
			if(errno == ENOENT) /* file dosen't exist */
			{
				/* get mode parameter */
				mode_t mode = va_arg(varargs, mode_t);
				/* attempt to create the file */
				inode = __otter_fs_touch(filename, dnode);
				if(!inode)
					return (-1);
				(*inode).permissions = (mode & 0x0FFF) | 0x3000; // TODO: use the umask?
                errno = 0;
			}
			else
				return (-1);
		}
		else
			return (-1);
	}
	else /* found the file */
	{
		if((O_CREAT & oflag) && (O_EXCL & oflag)) /* O_EXCL requires that the file did not exist with O_CREAT */
		{
			errno = EEXIST;
			return (-1);
		}
	}

	/* inode is valid */

	int fd = __otter_fs_open_file(inode, oflag);
	if(fd == -1)
		return (-1);

	if (O_TRUNC & oflag && (*inode).type == __otter_fs_TYP_FILE) /* O_TRUNC is ignored for special file types */
	{
		(*inode).size = 0;
		free((*inode).data);
		(*inode).data = NULL;
	}

	return (fd);
}
