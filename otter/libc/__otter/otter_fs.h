#ifndef _OTTER_FS_H
#define _OTTER_FS_H

#include <sys/socket.h>

#define __otter_fs_BLOCK_SIZE 256
#define __otter_fs_PIPE_SIZE 1024 /* this is often 64k, but that would be too big for Otter to have lots of copies of */

#define __otter_fs_TYP_FILE 0
#define __otter_fs_TYP_DIR 1

#define __otter_fs_TYP_FIFO 0x0040
#define __otter_fs_TYP_TTY  0x00C0
#define __otter_fs_TYP_NULL 0x0140
#define __otter_fs_TYP_ZERO 0x01C0
#define __otter_fs_TYP_SOCK 0x0240

#define __otter_fs_IS_TYP_SPECIAL(x) ((x) & 64)

#define __otter_fs_STATUS_OK 0
#define __otter_fs_STATUS_EOF 1

#define __otter_fs_MAXOPEN 64
#define __otter_fs_GLOBALMAXOPEN 128

#define NULL 0

struct __otter_fs_inode;
struct __otter_fs_filelist;
struct __otter_fs_dirlist;
struct __otter_fs_dnode;

struct __otter_fs_inode
{
	int linkno;
	int size;
	int numblocks;
	int type;
	int permissions;
	char* data;
	int r_openno;
	int w_openno;
};

struct __otter_fs_pipe_data
{
	int rhead; /* last read char */
	int whead; /* next char to write */
	char* data;
};

struct __otter_fs_sock_data
{
	struct sockaddr* addr; /* bound ip and port */
	int state; /* socket state machine */
	struct __otter_fs_pipe_data* recv_data;
	struct __otter_fs_sock_data* send_sock;
};

struct __otter_fs_filelist
{
	char* name;
	struct __otter_fs_inode* inode;
	struct __otter_fs_filelist* next;
};

struct __otter_fs_dirlist
{
	char* name;
	struct __otter_fs_dnode* dnode;
	struct __otter_fs_dirlist* next;
};

struct __otter_fs_dnode
{
	int linkno;
	int numdirs;
	int numfiles;
	struct __otter_fs_dirlist* dirs;
	struct __otter_fs_filelist* files;
	int permissions;
	int r_openno;
};

struct __otter_fs_dnode* __otter_fs_root;
struct __otter_fs_dnode* __otter_fs_pwd;

struct __otter_fs_inode* __otter_fs_find_inode_in_dir(const char* name, struct __otter_fs_dnode* dir);
struct __otter_fs_dnode* __otter_fs_find_dnode_in_dir(const char* name, struct __otter_fs_dnode* dir);
struct __otter_fs_inode* __otter_fs_find_inode_in_tree(char* name, struct __otter_fs_dnode* tree);
struct __otter_fs_dnode* __otter_fs_find_dnode_in_tree(char* name, struct __otter_fs_dnode* tree);
void* __otter_fs_find_vnode_in_tree(char* name, struct __otter_fs_dnode* tree, void* (*find_vnode)(char*, struct __otter_fs_dnode*));
struct __otter_fs_inode* __otter_fs_find_inode(const char* name);
struct __otter_fs_dnode* __otter_fs_find_dnode(const char* name);
void* __otter_fs_find_vnode(const char* name, void* (*find_vnode)(char*, struct __otter_fs_dnode*));

struct __otter_fs_open_file_table_entry
{
	int mode;
	int offset;
	int type;
	void* vnode;
	int openno;
	int status;
};

int* __otter_fs_fd_table;
struct __otter_fs_open_file_table_entry* __otter_fs_open_file_table;

int __otter_fs_next_fd();
int __otter_fs_more_fd(int arg);
int __otter_fs_next_global_fd();
int __otter_fs_open_file(struct __otter_fs_inode* inode, int mode);
int __otter_fs_open_dir(struct __otter_fs_dnode* dnode, int mode);
int __otter_fs_change_file_open_mode(int file, int mode);
int __otter_fs_change_dir_open_mode(int file, int mode);

int __otter_fs_legal_name(const char* name);

int __otter_fs_chmod_dir(int mode, struct __otter_fs_dnode* dir);
int __otter_fs_chmod_file(int mode, struct __otter_fs_inode* file);
struct __otter_fs_dnode* __otter_fs_mkdir(const char* name, struct __otter_fs_dnode* dir);
struct __otter_fs_inode* __otter_fs_touch(const char* name, struct __otter_fs_dnode* dir);
int __otter_fs_unlink_in_dir(const char* name, struct __otter_fs_dnode* dir);
int __otter_fs_rmdir_in_dir(const char* name, struct __otter_fs_dnode* dir);
int __otter_fs_link_file(const char* name, struct __otter_fs_inode* target, struct __otter_fs_dnode* dir);
int __otter_fs_link_dir(const char* name, struct __otter_fs_dnode* target, struct __otter_fs_dnode* dir);
int __otter_fs_change_open_mode(struct __otter_fs_open_file_table_entry* open_file, int mode);

void __otter_fs_mount();

// Utility functions
struct __otter_fs_dnode* find_filename_and_dnode(const char* path, char** basename);
struct __otter_fs_open_file_table_entry* get_open_file_from_fd(int fd);
struct __otter_fs_pipe_data* __otter_fs_init_new_pipe_data();
struct __otter_fs_sock_data* ___otter_fs_init_new_socket_data();
struct __otter_fs_inode* __otter_fs_init_new_socket();
void __otter_libc_free_socket(struct __otter_fs_inode* inode);

#endif

