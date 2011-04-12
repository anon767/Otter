#include <otter/otter_fs.h>
#include <otter/otter_builtins.h>
#include <otter/multiotter_builtins.h>

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/socket.h>

/** Returns a pointer to the dnode representing the path's directory,
		and sets the second argument to the path's basename. If the path contains
		no '/', the current directory is returned. If the path specifies
		an invalid directory, a null pointer is returned.

		Example:
		struct __otter_fs_dnode* dnode;
		char *filename;
		dnode = find_filename_and_dnode(path, &filename);
 */
struct __otter_fs_dnode* find_filename_and_dnode(const char* path, char** basename)
{
	const char *basename_finder = strrchr(path, '/');

	struct __otter_fs_dnode* dnode;
	if(basename_finder)
	{
		// Copy path so that we can truncate the string by writing a null in
		// place of the last '/'.
		char* name = strdup(path);

		name[basename_finder - path] = 0;

		*basename = basename_finder + 1;
		dnode = __otter_fs_find_dnode(name);
		free(name);
	}
	else
	{
		dnode = __otter_fs_pwd;
		*basename = path;
	}

	return dnode;
}

/** Returns the entry in the open file table that the given file descriptor
		refers to, or returns null if the file descriptor is invalid. */
struct __otter_fs_open_file_table_entry* get_open_file_from_fd(int fd)
{
	if(fd < 0 || fd >= __otter_fs_MAX_FDS)
	{
		errno = EBADF;
		return NULL;
	}
	int open_file_index = __otter_fs_fd_table[fd];
	if (open_file_index == -1)
	{
		errno = EBADF;
		return NULL;
	}
	return &__otter_fs_open_file_table[open_file_index];
}

struct __otter_fs_pipe_data* __otter_fs_init_new_pipe_data()
{
	struct __otter_fs_pipe_data* pipe = __otter_multi_gmalloc(sizeof(struct __otter_fs_pipe_data));
	pipe->rhead = __otter_fs_PIPE_SIZE - 1;
	pipe->whead = 0;
	pipe->data = __otter_multi_gmalloc(__otter_fs_PIPE_SIZE);
	return pipe;
}

struct __otter_fs_pipe_data *__otter_libc_get_pipe_data_from_open_file(struct __otter_fs_open_file_table_entry* open_file) {
	__ASSERT(open_file->type == __otter_fs_TYP_FIFO);
	return open_file->inode->data;
}

int __otter_fs_pipe_is_empty(struct __otter_fs_pipe_data *pipe) {
	return (pipe->rhead + 1) % __otter_fs_PIPE_SIZE == pipe->whead;
}

int __otter_fs_pipe_is_full(struct __otter_fs_pipe_data *pipe) {
	return pipe->rhead == pipe->whead;
}

struct __otter_fs_sock_data* __otter_fs_init_new_socket_data()
{
	struct __otter_fs_sock_data* sock = __otter_multi_gmalloc(sizeof(struct __otter_fs_sock_data));
	sock->addr = __otter_multi_gcalloc(__SOCKADDR_SHARED_LEN, 1);
	sock->state = 0;
	sock->options = 0;
	sock->recv_data = NULL; /* allocate this when connected or creating UDP */
	sock->sock_queue = NULL; /* allocate this when listen() or connect() */
	sock->backlog = 0;
	return sock;
}

struct __otter_fs_inode* __otter_fs_init_new_socket()
{
	struct __otter_fs_inode* inode = __otter_multi_gmalloc(sizeof(struct __otter_fs_inode));
	inode->linkno = 0;
	inode->size = 0;
	inode->type = __otter_fs_TYP_SOCK;
	inode->permissions = __otter_fs_umask | 0x01FF;
	inode->data = (void*)__otter_fs_init_new_socket_data();
	inode->r_openno = 0;
	inode->w_openno = 0;
	inode->numblocks = 0;
	return inode;
}

void __otter_fs_free_socket(struct __otter_fs_inode* inode)
{
	if(!inode)
		return;
	
	if(inode->data)
	{
		if(((struct __otter_fs_sock_data*)inode->data)->recv_data)
		{
			__otter_multi_gfree(((struct __otter_fs_sock_data*)inode->data)->recv_data->data);
			__otter_multi_gfree(((struct __otter_fs_sock_data*)inode->data)->recv_data);
		}
		__otter_multi_gfree(((struct __otter_fs_sock_data*)inode->data)->sock_queue);
		__otter_multi_gfree(((struct __otter_fs_sock_data*)inode->data)->addr);
		__otter_multi_gfree(inode->data);
	}
	__otter_multi_gfree(inode);
}


