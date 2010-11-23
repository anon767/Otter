#include <sys/stat.h>
#include <__otter/otter_fs.h>
#include <errno.h>

int __otter_libc_chmod(const char* name, mode_t mode)
{
	struct __otter_fs_inode* inode = __otter_fs_find_inode(name);
	if(!inode)
	{
		struct __otter_fs_dnode* dnode = __otter_fs_find_dnode(name);
		if(!dnode)
			return (-1);

		dnode = __otter_fs_chmod_dir(mode, dnode);
		if(!dnode)
			return (-1);
	}

	inode = __otter_fs_chmod_file(mode, inode);
	if(!inode)
		return (-1);

	return (0);
}

int __otter_libc_fchmod(int fd, mode_t mode)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	void* retval;
	if (open_file->type == __otter_fs_TYP_FILE)
	{
		retval = __otter_fs_chmod_file(mode, open_file->vnode);
	}
	else
	{
		retval = __otter_fs_chmod_dir(mode, open_file->vnode);
	}
	return retval;
}

mode_t __otter_fs_type_to_mode(int type)
{
	switch (type)
	{
		case __otter_fs_TYP_FILE: return (S_IFREG);
		case __otter_fs_TYP_DIR: return (S_IFDIR);
		case __otter_fs_TYP_FIFO: return (S_IFIFO);
		case __otter_fs_TYP_TTY: return (S_IFCHR);
		case __otter_fs_TYP_NULL: return (S_IFCHR);
		case __otter_fs_TYP_SOCK: return (S_IFSOCK);
		default: return (0);
	}

	return (0);
}

int __otter_libc_inode_stat(struct __otter_fs_inode* inode, struct stat* s)
{
	(*s).st_dev = 0;
	(*s).st_ino = (int)inode;
	(*s).st_mode = (*inode).permissions | __otter_fs_type_to_mode((*inode).type);
	(*s).st_nlink = (*inode).linkno;
	(*s).st_uid = ((*inode).permissions & S_ISUSR) >> 13; /* count bits from 0 */
	(*s).st_gid = ((*inode).permissions & S_ISGRP) >> 12;
	switch ((*inode).type)
	{
		case __otter_fs_TYP_TTY:
		case __otter_fs_TYP_NULL:
			(*s).st_rdev = (*inode).type;
			break;
		default: 
			(*s).st_rdev = 0;
			break;
	}
	(*s).st_size = (*inode).size;
	(*s).st_blksize = __otter_fs_BLOCK_SIZE;
	(*s).st_blocks = (*inode).numblocks;

	return (0);
}

int __otter_libc_dnode_stat(struct __otter_fs_dnode* dnode, struct stat* s)
{
	(*s).st_dev = 0;
	(*s).st_ino = (int)dnode;
	(*s).st_mode = (*dnode).permissions | S_IFDIR;
	(*s).st_nlink = (*dnode).linkno;
	(*s).st_uid = ((*dnode).permissions & S_ISUSR) >> 13;
	(*s).st_gid = ((*dnode).permissions & S_ISGRP) >> 12;
	(*s).st_rdev = 0;
	(*s).st_size = (*dnode).numfiles + (*dnode).numdirs;
	(*s).st_blksize = __otter_fs_BLOCK_SIZE;
	(*s).st_blocks = (((*dnode).numfiles + (*dnode).numdirs) / __otter_fs_BLOCK_SIZE) + 1;

	return (0);
}

int __otter_libc_fstat(int fd, struct stat* s)
{
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if (!open_file) return -1;

	if (open_file->type == __otter_fs_TYP_DIR)
	{
		struct __otter_fs_dnode* dnode = (struct __otter_fs_dnode*)open_file->vnode;
		return __otter_libc_dnode_stat(dnode, s);
	}

	struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
	return __otter_libc_inode_stat(inode, s);
}

int __otter_libc_lstat(const char* name, struct stat* s)
{
	struct __otter_fs_inode* inode = __otter_fs_find_inode(name);
	if(!inode)
	{
		struct __otter_fs_dnode* dnode = __otter_fs_find_dnode(name);
		if(!dnode)
			return (-1);

		return __otter_libc_dnode_stat(dnode, s);
	}

	return __otter_libc_inode_stat(inode, s);
}

int __otter_libc_mknod(const char* name, mode_t mode, dev_t dev)
{
	if(mode & S_IFCHR || mode & S_IFBLK) /* find out if dev makes sense */
	{
		switch (dev)
		{
			/* defined devices */
			case __otter_fs_TYP_TTY:
			case __otter_fs_TYP_NULL:
				break;
			default: /* invalid device */
				errno = EINVAL;
				return (-1);
				break;
		}
	} /* dev is ignored if this is not a character (or block) special file */
	else if(mode & S_IFIFO)
		dev = __otter_fs_TYP_FIFO;
	else if(mode & S_IFSOCK)
		dev = __otter_fs_TYP_SOCK;
	else
		dev = __otter_fs_TYP_FILE;

	char* a = name;

	while(*a != 0)
		a++;

	while(a != name && (*a) != '/')
		a--;
	
	struct __otter_fs_dnode* dnode;
	if(a == name)
	{
		dnode = __otter_fs_pwd;
	}
	else
	{
		*a = 0;
		dnode = __otter_fs_find_dnode(name);
		*a = '/';
	}

	if(!dnode)
		return (-1);

	a++;

	if(mode & S_IFDIR)
	{
		struct __otter_fs_dnode* newdir = __otter_fs_mkdir(a, dnode);

		if(!newdir)
			return (-1);

		(*newdir).permissions = (mode & 0x0FFF) | 0x3000;

		return (0);
	}

	struct __otter_fs_inode* newfile = __otter_fs_touch(a, dnode);

	if(!newfile)
		return (-1);
		
	if(mode & S_IFIFO)
	{
		struct __otter_fs_pipe_data* pipe = __otter_fs_init_new_pipe_data();
		newfile->data = (void*)pipe;
	}

	(*newfile).permissions = (mode & 0x0FFF) | 0x3000;
	(*newfile).type = dev;

	return (0);
}

int __otter_libc_mkdir(const char* name, mode_t mode)
{
	return mknod(name, mode | S_IFDIR, 0/*ignored*/);
}

int __otter_libc_mkfifo(const char* name, mode_t mode)
{
	return mknod(name, mode | S_IFIFO, 0/*ignored*/);
}

int __otter_libc_stat(const char* name, struct stat* s)
{
	return lstat(name, s);
}

mode_t __otter_libc_umask(mode_t mode)
{
	int old = __otter_fs_umask;
	__otter_fs_umask = (mode & 0x0FFF) | 0x3000;
	return old;
}

