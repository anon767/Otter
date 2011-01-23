#include <otter/otter_fs.h>
#include <otter/otter_user.h>

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

#define __otter_fs_error(e) {errno = (e); return NULL;}

/* file system access functions */

int __otter_fs_is_owner(int filep)
{
	return ((filep & 0x2000 && __otter_uid == __otter_UID_USER) || (__otter_uid == __otter_UID_ROOT));
}

int __otter_fs_is_group(int filep)
{
	return ((filep & 0x1000 && __otter_uid == __otter_GID_USER) || (__otter_uid == __otter_GID_ROOT));
}

int __otter_fs_can_permission(int filep, int wantp)
{
	if(__otter_fs_is_owner(filep))
		wantp = wantp << 6;
	else if(__otter_fs_is_group(filep))
		wantp = wantp << 3;

	return ((filep & wantp) == wantp);
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

struct __otter_fs_inode* __otter_fs_find_inode_in_tree(char* name, struct __otter_fs_dnode* tree)
{
	return (struct __otter_fs_inode*)__otter_fs_find_vnode_in_tree(name, tree, __otter_fs_find_inode_in_dir);
}

struct __otter_fs_dnode* __otter_fs_find_dnode_in_tree(char* name, struct __otter_fs_dnode* tree)
{
	return (struct __otter_fs_dnode*)__otter_fs_find_vnode_in_tree(name, tree, __otter_fs_find_dnode_in_dir);
}

void* __otter_fs_find_vnode_in_tree(char* name, struct __otter_fs_dnode* tree, void* (*find_vnode)(char*, struct __otter_fs_dnode*))
{
	if(!__otter_fs_can_permission((*tree).permissions, 1)) /* can't traverse dir */
		__otter_fs_error(EACCESS);

	char* s = strchr(name, '/');

	if(s == NULL) /* no leading directory */
	{
		return find_vnode(name, tree);
	}

	/* otherwise recursivly decend file system */
	*s = 0; /* change the first '/' into an end of string */
	struct __otter_fs_dnode* d = __otter_fs_find_dnode_in_dir(name, tree);
	*s = '/'; /* change it back */

	if(d == NULL) /* no such directory */
	{
		__otter_fs_error(ENOENT); /* cannot find file */
	}

	while(*s == '/')
	{
		s++; /* skip any extra '/' between dirs, posix says they should be ignored */
		if(*s == 0)
			__otter_fs_error(ENOENT); /* path ended in a directory name not a file */
	}

	return __otter_fs_find_vnode_in_tree(s, d, find_vnode); /* search for the shortened path in the subtree */
}

struct __otter_fs_inode* __otter_fs_find_inode(const char* name)
{
	return (struct __otter_fs_inode*)__otter_fs_find_vnode(name, __otter_fs_find_inode_in_dir);
}

struct __otter_fs_dnode* __otter_fs_find_dnode(const char* name)
{
	return (struct __otter_fs_dnode*)__otter_fs_find_vnode(name, __otter_fs_find_dnode_in_dir);
}

void* __otter_fs_find_vnode(const char* name_in, void* (*find_vnode)(char*, struct __otter_fs_dnode*))
{
	char* name = malloc(__libc_get_block_size(name_in));
	strcpy(name, name_in);

	if(name)
	{
		void* vnode;
		if(*name == '/') /* absolute path */
		{
			name++;
			vnode = __otter_fs_find_vnode_in_tree(name, __otter_fs_root, find_vnode);
		}
		else /* relative path */
			vnode = __otter_fs_find_vnode_in_tree(name, __otter_fs_pwd, find_vnode);

		free(name);
		return vnode;
	}

	__otter_fs_error(ENOENT);
}

int __otter_fs_legal_char(char c)
{
	return (isalnum(c) | c == '-' | c == '_' | c == '.');
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
			if(*name == 0)
				return (1);
			if(!__otter_fs_legal_char(*name))
				return (0);
		}

	}
	
	return (0);
}

int __otter_fs_chmod_dir(int mode, struct __otter_fs_dnode* dir)
{
	if(__otter_fs_is_owner((*dir).permissions)) /* can't chmod unless user owns */
		__otter_fs_error(EPERM);

	(*dir).permissions = mode & 0x0FFF;

	return(1);
}

int __otter_fs_chmod_file(int mode, struct __otter_fs_inode* file)
{
	if(__otter_fs_is_owner((*file).permissions)) /* can't chmod unless user owns */
		__otter_fs_error(EPERM);

	(*file).permissions = mode & 0x0FFF;

	return(1);
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

		struct __otter_fs_dnode* newdir = __otter_multi_gmalloc(sizeof(struct __otter_fs_dnode));
		struct __otter_fs_dirlist* basic_dir_entries1 = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
		struct __otter_fs_dirlist* basic_dir_entries0 = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
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
		(*newdir).r_openno = 0;
		(*newdir).permissions = __otter_fs_umask;
		struct __otter_fs_dirlist* newdirlist = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
		(*newdirlist).name = name;
		(*newdirlist).dnode = newdir;
		(*newdirlist).next = NULL;

		(*dir).numdirs++;

		if(pdirs)
			(*pdirs).next = newdirlist;
		else
			(*dir).dirs = newdirlist;

		return newdir;
	}

	__otter_fs_error(EINVAL);
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

		struct __otter_fs_filelist* files = (*dir).files;
		struct __otter_fs_filelist* pfiles = NULL;

		while(files)
		{
			pfiles = files;
			files = (*files).next;
		}

		struct __otter_fs_inode* newfile = __otter_multi_gmalloc(sizeof(struct __otter_fs_inode));
		(*newfile).linkno = 1;
		(*newfile).size = 0;
		(*newfile).type = __otter_fs_TYP_FILE;
		(*newfile).permissions = __otter_fs_umask;
		(*newfile).data = NULL;
		(*newfile).numblocks = 0;
		(*newfile).r_openno = 0;
		(*newfile).w_openno = 0;
		struct __otter_fs_filelist* newfilelist = __otter_multi_gmalloc(sizeof(struct __otter_fs_filelist));
		(*newfilelist).name = name;
		(*newfilelist).inode = newfile;
		(*newfilelist).next = NULL;

		(*dir).numfiles++;

		if(pfiles)
			(*pfiles).next = newfilelist;
		else
			(*dir).files = newfilelist;

		return newfile;
	}

	__otter_fs_error(EINVAL);
}

int __otter_fs_unlink_in_dir(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_find_dnode_in_dir(name, dir)) /* name is a dir */
		__otter_fs_error(EPERM);

	if((*dir).numfiles == 0) /* no files in dir */
		__otter_fs_error(ENOENT);

	struct __otter_fs_filelist* files = (*dir).files;
	struct __otter_fs_filelist* pfiles = NULL;

	while(files)
	{
		int cmp = strcmp((*files).name, name);
		if(cmp == 0) /* found the file */
		{
			struct __otter_fs_inode* inode = (*files).inode;
			(*inode).linkno--;

			if(pfiles == NULL) /* file was the first entry */
			{
				(*dir).files = (*files).next;
			}
			else
			{
				(*pfiles).next = (*files).next;
			}

			if((*inode).linkno == 0) /* all links to file are removed, file is deleated */
			{
				if(!((*inode).r_openno | (*inode).w_openno)) /* file isn't open; free contents */
				{
					free((*inode).data);
					free(inode);
				}
			}

			return (1);
		}
		pfiles = files;
		files = (*files).next;
	}

	__otter_fs_error(ENOENT);

}

int __otter_fs_rmdir_in_dir(const char* name, struct __otter_fs_dnode* dir)
{
	if(!__otter_fs_can_permission((*dir).permissions, 2)) /* can't write to dir */
		__otter_fs_error(EACCESS);

	if(__otter_fs_find_inode_in_dir(name, dir)) /* name is a dir */
		__otter_fs_error(ENOTDIR);

	if((*dir).numdirs == 0) /* no directories in dir */
		__otter_fs_error(ENOENT);

	struct __otter_fs_dirlist* dirs = (*dir).dirs;
	struct __otter_fs_dirlist* pdirs = NULL;

	while(dirs)
	{
		int cmp = strcmp((*dirs).name, name);
		if(cmp == 0) /* found the file */
		{
			struct __otter_fs_dnode* dnode = (*dirs).dnode;

			if((*dnode).numfiles || (*dnode).numdirs) /* directory is not empty */
				__otter_fs_error(ENOTEMPTY);

			(*dnode).linkno--;

			if(pdirs == NULL) /* directory was the first entry */
			{
				(*dir).dirs = (*dirs).next;
			}
			else
			{
				(*pdirs).next = (*dirs).next;
			}

			if((*dnode).linkno == 0) /* all links to directory are removed, file is deleated */
			{
				if(!((*dnode).r_openno)) /* directory isn't open; free contents */
				{
					free(dnode);
				}
			}

			return (1);
		}
		pdirs = dirs;
		dirs = (*dirs).next;
	}

	__otter_fs_error(ENOENT);

}

int __otter_fs_link_file(const char* name, struct __otter_fs_inode* target, struct __otter_fs_dnode* dir)
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

		struct __otter_fs_filelist* newfilelist = __otter_multi_gmalloc(sizeof(struct __otter_fs_filelist));
		(*newfilelist).name = name;
		(*newfilelist).inode = target;
		(*newfilelist).next = NULL;

		(*target).linkno++;

		(*dir).numfiles++;
		if(pfiles)
			(*pfiles).next = newfilelist;
		else
			(*dir).files = newfilelist;

		return(1);
	}

	__otter_fs_error(EINVAL);
}

int __otter_fs_link_dir(const char* name, struct __otter_fs_dnode* target, struct __otter_fs_dnode* dir)
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

		struct __otter_fs_dirlist* newdirlist = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
		(*newdirlist).name = name;
		(*newdirlist).dnode = target;
		(*newdirlist).next = NULL;

		(*target).linkno++;

		(*dir).numdirs++;
		if(pdirs)
			(*pdirs).next = newdirlist;
		else
			(*dir).dirs = newdirlist;

		return(1);
	}

	__otter_fs_error(EINVAL);
}

/* file descriptors */

/* first free file descriptor */
int __otter_fs_next_fd()
{
	for(int i = 0; i < __otter_fs_MAXOPEN; i++)
	{
		if(__otter_fs_fd_table[i] == -1)
			return (i);
	}

	return (-1);
}

/* first free file greater than arg */
int __otter_fs_more_fd(int arg)
{
	for(int i = arg; i < __otter_fs_MAXOPEN; i++)
	{
		if(__otter_fs_fd_table[i] == -1)
			return (i);
	}

	return (-1);
}

/* first free file table entry */
int __otter_fs_next_global_fd()
{
	for(int i = 0; i < __otter_fs_GLOBALMAXOPEN; i++)
	{
		if(__otter_fs_open_file_table[i].openno == 0)
			return (i);
	}

	return (-1);
}
#include<__otter/otter_scheduler.h>
int __otter_fs_open_file(struct __otter_fs_inode* inode, int mode)
{
	__otter_multi_begin_atomic();
	int permissions = 0;
	if(mode & O_RDONLY)
		permissions += 2;
	if(mode & O_WRONLY)
		permissions += 4;

	if(!__otter_fs_can_permission((*inode).permissions, permissions))
	{
		errno = EACCESS;
		__otter_multi_end_atomic();
		return (-1);
	}

	int fd = __otter_fs_next_fd();
	if(fd == -1)
	{
		errno = EMFILE;
		__otter_multi_end_atomic();
		return (-1);
	}

	int ft = __otter_fs_next_global_fd();
	if(ft == -1)
	{
		errno = ENFILE;
		__otter_multi_end_atomic();
		return (-1);
	}

	if(mode & O_RDONLY)
		(*inode).r_openno++;
	if(mode & O_WRONLY)
		(*inode).w_openno++;

	__otter_fs_fd_table[fd] = ft;
	__otter_fs_open_file_table[ft].mode = mode;
	__otter_fs_open_file_table[ft].type = (*inode).type;
	__otter_fs_open_file_table[ft].vnode = (void*)inode;
	__otter_fs_open_file_table[ft].offset = 0;
	__otter_fs_open_file_table[ft].openno = 1;
	__otter_fs_open_file_table[ft].status = __otter_fs_STATUS_OK;

	if((*inode).size == 0)
		__otter_fs_open_file_table[ft].status = __otter_fs_STATUS_EOF;
	
	__otter_multi_end_atomic();
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

	(*dnode).r_openno++;

	__otter_fs_fd_table[fd] = ft;
	__otter_fs_open_file_table[ft].mode = mode;
	__otter_fs_open_file_table[ft].type = __otter_fs_TYP_DIR;
	__otter_fs_open_file_table[ft].vnode = (void*)dnode;
	__otter_fs_open_file_table[ft].offset = 0;
	__otter_fs_open_file_table[ft].openno = 1;
	__otter_fs_open_file_table[ft].status = __otter_fs_STATUS_OK;

	return (fd);
}

int __otter_fs_change_open_mode(struct __otter_fs_open_file_table_entry* open_file, int mode)
{
	int old_mode = (*open_file).mode;
	int changed = old_mode ^ mode;
	int add = changed & mode;
	int remove = changed & old_mode;

	int permissions = 0;
	if(add & O_RDONLY)
		permissions += 2;
	if(add & O_WRONLY)
	{
		permissions += 4;

		if((*open_file).type == __otter_fs_TYP_DIR) /* can't have write access to a directory */
		{
			errno = EISDIR;
			return (-1);
		}
	}

	if((*open_file).type == __otter_fs_TYP_FILE)
	{
		struct __otter_fs_inode* inode = (struct __otter_fs_inode*)((*open_file).vnode);

		if(!__otter_fs_can_permission((*inode).permissions, permissions))
		{
			errno = EACCESS;
			return (-1);
		}

		if(add & O_RDONLY)
			(*inode).r_openno++;
		if(add & O_WRONLY)
			(*inode).w_openno++;

		if(remove & O_RDONLY)
			(*inode).r_openno--;
		if(remove & O_WRONLY)
			(*inode).w_openno--;
	}
	else
	{
		struct __otter_fs_dnode* dnode = (struct __otter_fs_dnode*)((*open_file).vnode);

		if(!__otter_fs_can_permission((*dnode).permissions, permissions))
		{
			errno = EACCESS;
			return (-1);
		}

		if(add & O_RDONLY)
			(*dnode).r_openno++;

		if(remove & O_RDONLY)
			(*dnode).r_openno--;
	}

	(*open_file).mode = mode;

	return(1);
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
	struct __otter_fs_dnode* newdir = __otter_multi_gmalloc(sizeof(struct __otter_fs_dnode));
	struct __otter_fs_dirlist* basic_dir_entries1 = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
	struct __otter_fs_dirlist* basic_dir_entries0 = __otter_multi_gmalloc(sizeof(struct __otter_fs_dirlist));
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
	struct __otter_fs_inode* zero = __otter_fs_touch("zero", dev);
	__otter_fs_link_file("console", tty, dev);

	(*tty).permissions = 0x01B6;
	(*null).permissions = 0x01B6;
	(*zero).permissions = 0x01B6;

	(*tty).type = __otter_fs_TYP_TTY;
	(*null).type = __otter_fs_TYP_NULL;
	(*zero).type = __otter_fs_TYP_ZERO;

	(*wrk).permissions = 0x01FF;
	(*tmp).permissions = 0x03FF;
	(*dev).permissions = 0x01FF;
	(*root).permissions = 0x01ED;

	__otter_fs_root = root;
	__otter_fs_pwd = wrk;

	/* mark all file descriptors and file table entries as unused */
	__otter_fs_fd_table = malloc(sizeof(int)*__otter_fs_MAXOPEN); /* local */
	memset(__otter_fs_fd_table, -1, __otter_fs_MAXOPEN*sizeof(int));
	__otter_fs_open_file_table = __otter_multi_gmalloc(sizeof(struct __otter_fs_open_file_table_entry)*__otter_fs_GLOBALMAXOPEN);
	memset(__otter_fs_open_file_table, 0, __otter_fs_GLOBALMAXOPEN*sizeof(struct __otter_fs_open_file_table_entry));
}

