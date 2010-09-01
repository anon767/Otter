#include <__otter/otter_fs.h>

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

#define __otter_fs_MAXOPEN 64
#define __otter_fs_GLOBALMAXOPEN 128

#define __otter_fs_error(e) {errno = (e); return NULL;}

/* file system access functions */

int __otter_fs_can_permission(int filep, int wantp)
{
	if(filep & 0x2000)
		wantp = wantp << 6;
	else if(filep & 0x1000)
		wantp = wantp << 3;

	return (filep & wantp == wantp);
}

int __otter_fs_is_owner(int filep)
{
	return (filep & 0x2000);
}

int __otter_fs_is_group(int filep)
{
	return (filep & 0x1000);
}

struct __otter_fs_inode* __otter_fs_find_inode_in_dir(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 1)) /* can't traverse dir */
		__otter_fs_error(EACCESS);

	if((*dir).numfiles == 0) /* no files in dir */
		__otter_fs_error(ENOENT);

	struct __otter_fs_filelist* files = (*dir).files;

	while(files)
	{
		int cmp = strcmp((*files).name, name);
		if(cmp == 0) /* found the file */
			return (*files).inode;
		files = (*files).next;
	}

	__otter_fs_error(ENOENT);
}

struct __otter_fs_dnode* __otter_fs_find_dnode_in_dir(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 1)) /* can't traverse dir */
		__otter_fs_error(EACCESS);

	if((*dir).numdirs == 0) /* no dirs in dir */
		__otter_fs_error(ENOENT);

	struct __otter_fs_dirlist* dirs = (*dir).dirs;

	while(dirs)
	{
		int cmp = strcmp((*dirs).name, name);
		if(cmp == 0) /* found the file */
			return (*dirs).dnode;
		dirs = (*dirs).next;
	}

	__otter_fs_error(ENOENT);
}

struct __otter_fs_inode* __otter_fs_find_inode_in_tree(const char* name, struct __otter_fs_dnode* tree)
{
	if(!__otter_fs_can_permission((*tree).permissions, 1)) /* can't traverse dir */
		__otter_fs_error(EACCESS);

	char* s = strchr(name, '/');

	if(s == NULL) /* no leading directory */
	{
		return __otter_fs_find_inode_in_dir(name, tree);
	}

	/* otherwise recursivly decend file system */
	*s = 0; /* change the first '/' into an end of string */
	struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, tree);
	*s = '/'; /* change it back */

	if(d == 0); /* no such directory */
	{
		__otter_fs_error(ENOENT); /* cannot find file */
	}

	while(*s == '/')
	{
		if(*s == 0)
			__otter_fs_error(ENOENT); /* path ended in a directory name not a file */
		s++; /* skip any extra '/' between dirs, posix says they should be ignored */
	}

	return __otter_fs_find_inode_in_tree(s, d); /* search for the shortened path in the subtree */
}

struct __otter_fs_dnode* __otter_fs_find_dnode_in_tree(const char* name, struct __otter_fs_dnode* tree)
{
	if(!__otter_fs_can_permission((*tree).permissions, 1)) /* can't traverse dir */
		__otter_fs_error(EACCESS);

	char* s = strchr(name, '/');

	if(s == NULL) /* no leading directory */
	{
		return __otter_fs_find_dnode_in_dir(name, tree);
	}

	/* otherwise recursivly decend file system */
	*s = 0; /* change the first '/' into an end of string */
	struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, tree);
	*s = '/'; /* change it back */

	if(d == 0); /* no such directory */
	{
		__otter_fs_error(ENOENT); /* cannot find file */
	}

	while(*s == '/')
	{
		if(*s == 0)
			__otter_fs_error(ENOENT); /* path ended in a directory name not a file */
		s++; /* skip any extra '/' between dirs, posix says they should be ignored */
	}

	return __otter_fs_find_dnode_in_tree(s, d); /* search for the shortened path in the subtree */
}

struct __otter_fs_inode* __otter_fs_find_inode(const char* name)
{
	if(name)
	{
		if(*name == '/') /* absolute path */
			return __otter_fs_find_inode_in_tree(name, __otter_fs_root);
		else /* relative path */
			return __otter_fs_find_inode_in_tree(name, __otter_fs_pwd);
	}

	__otter_fs_error(ENOENT);
}

struct __otter_fs_dnode* __otter_fs_find_dnode(const char* name)
{
	if(name)
	{
		if(*name == '/') /* absolute path */
			return __otter_fs_find_dnode_in_tree(name, __otter_fs_root);
		else /* relative path */
			return __otter_fs_find_dnode_in_tree(name, __otter_fs_pwd);
	}

	__otter_fs_error(ENOENT);
}

int __otter_fs_legal_char(char c)
{
	return (isalnum(c) | c == '-');
}

/* OtterFS only allows posix portable names */
int __otter_fs_legal_name(const char* name)
{
	if(name)
	{
		if(*name == 0) /*  must have at least one character */
			return (0);
		if(*name == '-') /* cannot start with '-' */
			return (0);
			
		for(; ; name++)
		{
			if(!__otter_fs_legal_char(*name))
				return (0);
			if(*name == 0)
				return (1);
		}

	}
	
	return (0);
}

struct __otter_fs_dnode* __otter_fs_chmod_dir(int mode, struct __otter_fs_dnode* dir)
{
	if(__otter_fs_is_owner((*dir).permissions)) /* can't chmod unless user owns */
		__otter_fs_error(EPERM);

	(*dir).permissions = mode & 0x0FFF;

	return dir;
}

struct __otter_fs_inode* __otter_fs_chmod_file(int mode, struct __otter_fs_inode* file)
{
	if(__otter_fs_is_owner((*file).permissions)) /* can't chmod unless user owns */
		__otter_fs_error(EPERM);

	(*file).permissions = mode & 0x0FFF;

	return file;
}

int __otter_fs_umask = 0x31ED;

struct __otter_fs_dnode* __otter_fs_mkdir(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_legal_name(name))
	{
		struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, dir);
		struct __otter_fs_inode* f = __otter_fs_find_inode_in_dir(name, dir);
		if(d || f) /* file or directory already exists */
			__otter_fs_error(EEXIST);

		struct __otter_fs_dirlist* dirs = (*dir).dirs;
		struct __otter_fs_dirlist* pdirs = NULL;

		while(dirs)
		{
			pdirs = dirs;
			dirs = (*dirs).next;
		}

		struct __otter_fs_dnode* newdir = malloc(sizeof(struct __otter_fs_dnode));
		struct __otter_fs_dirlist* basic_dir_entries1 = malloc(sizeof(struct __otter_fs_dirlist));
		struct __otter_fs_dirlist* basic_dir_entries0 = malloc(sizeof(struct __otter_fs_dirlist));
		(*basic_dir_entries1).name = "..";
		(*basic_dir_entries1).dnode = dir;
		(*basic_dir_entries1).next = NULL;
		(*basic_dir_entries0).name = ".";
		(*basic_dir_entries0).dnode = newdir;
		(*basic_dir_entries0).next = basic_dir_entries1;
		(*newdir).linkno = 1; /* don't count . and .. */
		(*newdir).numfiles = 0;
		(*newdir).numdirs = 2;
		(*newdir).files = NULL;
		(*newdir).dirs = basic_dir_entries0;
		(*newdir).permissions = __otter_fs_umask;
		struct __otter_fs_dirlist* newdirlist = malloc(sizeof(struct __otter_fs_dirlist));
		(*newdirlist).name = name;
		(*newdirlist).dnode = newdir;
		(*newdirlist).next = NULL;

		(*dir).numdirs++;
		(*dirs).next = newdirlist;

		return newdir;
	}

	return NULL;
}

struct __otter_fs_inode* __otter_fs_touch(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_legal_name(name))
	{
		struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, dir);
		struct __otter_fs_inode* f = __otter_fs_find_inode_in_dir(name, dir);
		if(d || f) /* file or directory already exists */
			__otter_fs_error(EEXIST);

		struct __otter_fs_dirlist* files = (*dir).files;
		struct __otter_fs_dirlist* pfiles = NULL;

		while(files)
		{
			pfiles = files;
			files = (*files).next;
		}

		struct __otter_fs_inode* newfile = malloc(sizeof(struct __otter_fs_inode));
		(*newfile).linkno = 1;
		(*newfile).size = 0;
		(*newfile).type = __otter_fs_TYP_FILE;
		(*newfile).permissions = __otter_fs_umask;
		(*newfile).data = NULL;
		(*newfile).numblocks = 0;
		struct __otter_fs_filelist* newfilelist = malloc(sizeof(struct __otter_fs_filelist));
		(*newfilelist).name = name;
		(*newfilelist).inode = newfile;
		(*newfilelist).next = NULL;

		(*dir).numfiles++;
		(*files).next = newfilelist;

		return newfile;
	}

	return NULL;
}

struct __otter_fs_inode* __otter_fs_link_file(const char* name, struct __otter_fs_inode* target, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_legal_name(name))
	{
		struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, dir);
		struct __otter_fs_inode* f = __otter_fs_find_inode_in_dir(name, dir);
		if(d || f) /* file or directory already exists */
			return NULL;

		struct __otter_fs_dirlist* files = (*dir).files;
		struct __otter_fs_dirlist* pfiles = NULL;

		while(files)
		{
			pfiles = files;
			files = (*files).next;
		}

		struct __otter_fs_filelist* newfilelist = malloc(sizeof(struct __otter_fs_filelist));
		(*newfilelist).name = name;
		(*newfilelist).inode = target;
		(*newfilelist).next = NULL;

		(*target).linkno++;

		(*dir).numfiles++;
		(*files).next = newfilelist;

		return target;
	}

	return NULL;
}

struct __otter_fs_dnode* __otter_fs_link_dir(const char* name, struct __otter_fs_dnode* target, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_legal_name(name))
	{
		struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, dir);
		struct __otter_fs_inode* f = __otter_fs_find_inode_in_dir(name, dir);
		if(d || f) /* file or directory already exists */
			return NULL;

		struct __otter_fs_dirlist* dirs = (*dir).dirs;
		struct __otter_fs_dirlist* pdirs = NULL;

		while(dirs)
		{
			pdirs = dirs;
			dirs = (*dirs).next;
		}

		struct __otter_fs_dirlist* newdirlist = malloc(sizeof(struct __otter_fs_dirlist));
		(*newdirlist).name = name;
		(*newdirlist).dnode = target;
		(*newdirlist).next = NULL;

		(*target).linkno++;

		(*dir).numdirs++;
		(*dirs).next = newdirlist;

		return target;
	}

	return NULL;
}

/* file descriptors */

/* first free file descriptor */
int __otter_fs_next_fd()
{
	for(int i = 0; i < __otter_fs_MAXOPEN; i++)
	{
		if(__otter_fs_files[i] == -1)
			return (i);
	}

	return (-1);
}

/* first free file table entry */
int __otter_fs_next_global_fd()
{
	for(int i = 0; i < __otter_fs_GLOBALMAXOPEN; i++)
	{
		if(__otter_fs_open_files[i].openno == 0)
			return (i);
	}

	return (-1);
}

int __otter_fs_open_file(struct __otter_fs_inode* inode, int mode)
{
	int permissions = 0;
	if(mode & O_RDONLY)
		permissions += 2;
	if(mode & O_WRONLY)
		permissions += 4;

	if(!__otter_fs_can_permission((*inode).permissions, permissions))
	{
		errno = EACCESS;
		return (-1);
	}

	int fd = __otter_fs_next_fd();
	if(fd == -1)
	{
		errno = EMFILE;
		return (-1);
	}

	int ft = __otter_fs_next_global_fd();
	if(ft == -1)
	{
		errno = ENFILE;
		return (-1);
	}

	__otter_fs_files[fd] = ft;
	__otter_fs_open_files[ft].mode = mode;
	__otter_fs_open_files[ft].type = (*inode).type;
	__otter_fs_open_files[ft].vnode = (void*)inode;
	__otter_fs_open_files[ft].offset = 0;
	__otter_fs_open_files[ft].openno = 1;
	__otter_fs_open_files[ft].status = __otter_fs_STATUS_OK;

	if((*inode).size == 0)
		__otter_fs_open_files[ft].status = __otter_fs_STATUS_EOF;

	return (fd);
}

int __otter_fs_open_dir(struct __otter_fs_dnode* dnode, int mode)
{
	int permissions = 0;
	if(mode & O_RDONLY)
		permissions = 2;
	if(mode & O_WRONLY) /* can't have write access to a directory */
	{
		errno = EISDIR;
		return (-1);
	}

	if(!__otter_fs_can_permission((*dnode).permissions, permissions))
	{
		errno = EACCESS;
		return (-1);
	}

	int fd = __otter_fs_next_fd();
	if(fd == -1)
	{
		errno = EMFILE;
		return (-1);
	}

	int ft = __otter_fs_next_global_fd();
	if(ft == -1)
	{
		errno = ENFILE;
		return (-1);
	}

	__otter_fs_files[fd] = ft;
	__otter_fs_open_files[ft].mode = mode;
	__otter_fs_open_files[ft].type = __otter_fs_TYP_DIR;
	__otter_fs_open_files[ft].vnode = (void*)dnode;
	__otter_fs_open_files[ft].offset = 0;
	__otter_fs_open_files[ft].openno = 1;
	__otter_fs_open_files[ft].status = __otter_fs_STATUS_OK;

	return (fd);
}

struct __otter_fs_inode* __otter_fs_get_inode_from_fd(int file)
{
	if(file > -1 && file < __otter_fs_MAXOPEN) /* is file a possible valid file? */
	{
		int globalfile = __otter_fs_files[file];

		if(globalfile > -1 && globalfile < __otter_fs_GLOBALMAXOPEN) /* is file a valid file entry? */
		{
			if(__otter_fs_open_files[globalfile].type == __otter_fs_TYP_FILE)
			{
				return ((struct __otter_fs_inode*)__otter_fs_open_files[globalfile].vnode);
			}
		}
	}

	__otter_fs_error(EBADF);
}

struct __otter_fs_dnode* __otter_fs_get_dnode_from_fd(int file)
{
	if(file > -1 && file < __otter_fs_MAXOPEN) /* is file a possible valid file? */
	{
		int globalfile = __otter_fs_files[file];

		if(globalfile > -1 && globalfile < __otter_fs_GLOBALMAXOPEN) /* is file a valid file entry? */
		{
			if(__otter_fs_open_files[globalfile].type == __otter_fs_TYP_DIR)
			{
				return ((struct __otter_fs_dnode*)__otter_fs_open_files[globalfile].vnode);
			}
		}
	}

	__otter_fs_error(EBADF);
}

/* file system initilization */

/*
	/-+--dev/-+--null
	  |       +--tty
	  |       +--console
	  +--tmp/
          +--wrk/
*/

struct __otter_fs_dnode* __otter_fs_mkroot()
{
	struct __otter_fs_dnode* newdir = malloc(sizeof(struct __otter_fs_dnode));
	struct __otter_fs_dirlist* basic_dir_entries1 = malloc(sizeof(struct __otter_fs_dirlist));
	struct __otter_fs_dirlist* basic_dir_entries0 = malloc(sizeof(struct __otter_fs_dirlist));
	(*basic_dir_entries1).name = "..";
	(*basic_dir_entries1).dnode = newdir;
	(*basic_dir_entries1).next = NULL;
	(*basic_dir_entries0).name = ".";
	(*basic_dir_entries0).dnode = newdir;
	(*basic_dir_entries0).next = basic_dir_entries1;
	(*newdir).linkno = 1;
	(*newdir).numfiles = 0;
	(*newdir).numdirs = 2;
	(*newdir).files = NULL;
	(*newdir).dirs = basic_dir_entries0;
	(*newdir).permissions = 0x31ED;

	return newdir;
}

void __otter_fs_mount()
{
	struct __otter_fs_dnode* root = __otter_fs_mkroot();
	struct __otter_fs_dnode* wrk = __otter_fs_mkdir("wrk", root);
	struct __otter_fs_dnode* tmp = __otter_fs_mkdir("tmp", root);
	struct __otter_fs_dnode* dev = __otter_fs_mkdir("dev", root);

	struct __otter_fs_inode* tty = __otter_fs_touch("tty", dev);
	struct __otter_fs_inode* null = __otter_fs_touch("null", dev);
	__otter_fs_link_file("console", tty, dev);

	(*tty).permissions = 0x01B6;
	(*null).permissions = 0x01B6;

	(*wrk).permissions = 0x01FF;
	(*tmp).permissions = 0x03FF;
	(*dev).permissions = 0x01FF;
	(*root).permissions = 0x01ED;

	__otter_fs_root = root;
	__otter_fs_pwd = wrk;

	/* mark all file descriptors and file table entries as unused */
	__otter_fs_files = malloc(sizeof(int)*__otter_fs_MAXOPEN);
	memset(__otter_fs_files, -1, __otter_fs_MAXOPEN*sizeof(int));
	__otter_fs_open_files = malloc(sizeof(struct __otter_fs_ft)*__otter_fs_GLOBALMAXOPEN);
	memset(__otter_fs_open_files, 0, __otter_fs_GLOBALMAXOPEN*sizeof(struct __otter_fs_ft));
}

