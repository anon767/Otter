#include <fcntl.h>
#include <__otter/otter_fs.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int __otter_libc_creat(const char* name, mode_t mode)
{
	return open(name, O_WRONLY | O_CREAT | O_TRUNC, mode);
}

/* TODO: when teh functions this should be calling are done finish this one */
int __otter_libc_fcntl(int fd, int cmd, ...)
{
	va_list varargs;
	va_start(varargs, cmd);

	int r = -1;
	
	switch (cmd)
	{
		case F_DUPFD:
			break;
		case F_GETFD: 
			break;
		case F_SETFD: 
			break;
		case F_GETFL:
			if(fd > -1 && fd < __otter_fs_MAXOPEN) /* is file a possible valid file? */
			{
				int ft = __otter_fs_files[fd];

				if(ft > -1) /* is file a valid file entry? */
				{
					r = __otter_fs_open_files[ft].mode;
				}
			}
			break;
		case F_SETFL:;
			int mode = va_arg(varargs, int);

			if(fd > -1 && fd < __otter_fs_MAXOPEN) /* is file a possible valid file? */
			{
				int ft = __otter_fs_files[fd];

				if(ft > -1) /* is file a valid file entry? */
				{
					r = __otter_fs_change_open_mode(fd, mode) - 1;
				}
			}
			break;
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

int __otter_libc_open(const char* name2, int oflag, ...)
{
	// alloca so we don't have to worry about calling free
	char* name = alloca(__libc_get_block_size(name2));
	strcpy(name, name2);

	if(O_NONBLOCK & oflag) /* non blocking I/O not supported */
	{
		__ASSERT(0);
	}

	if (O_TRUNC & oflag && !(O_WRONLY & oflag)) /* undefined behavior */
	{
		assert(0);
		return (-1);
	}

	struct __otter_fs_dnode* dnode = __otter_fs_find_dnode(name);
	if(dnode) /* is name a directory that already exists? */
	{
		if (O_TRUNC & oflag) /* undefined behavior */
		{
			assert(0);
			return (-1);
		}

		return __otter_fs_open_dir(dnode, oflag);
	}

	/* we are looking for a file */
	
	char* a = strrchr(name,'/');

	if(a)
	{
		*a = 0;
		dnode = __otter_fs_find_dnode(name);
		// We never use name again, so we don't have to restore the '/' that we wrote over
		a++;
	}
	else
	{
		dnode = __otter_fs_pwd;
		a = name;
	}

	if(!dnode) /* can't find path */
	{
		errno = ENOENT;
		return (-1);
	}

	struct __otter_fs_inode* inode = __otter_fs_find_inode_in_dir(a, dnode);
	if(!inode) /* file not found or need permission to browse dir */
	{
		if(O_CREAT & oflag) /* should the file be created? */
		{
			if(errno != EACCESS) /* file dosen't exist */
			{
				/* get mode parameter */
				va_list varargs;
				va_start(varargs, oflag);
				mode_t mode = va_arg(varargs, mode_t);
				va_end(varargs);
				/* attempt to create the file */
				inode = __otter_fs_touch(a, dnode);
				if(!inode)
					return (-1);
				(*inode).permissions = (mode & 0x0FFF) | 0x3000;
			}
			else
				return (-1);
		}
		else
			return (-1);
	}
	else /* found the file */
	{
		if((O_CREAT | O_EXCL) & oflag) /* O_EXCL requires that the file did not exist with O_CREAT */
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
