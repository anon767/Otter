#include <__otter/otter_fs.h>

#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

/** Returns a pointer to the dnode representing the path's directory,
		and sets the second argument to the path's basename. If the path contains
		no '/', the current directory is returned. If the path specifies
		an invalid directory, a null pointer is returned.

		Example:
		__otter_fs_dnode* dnode;
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
		char* name = malloc(strlen(path) + 1);
		strcpy(name, path);

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
	if(fd < 0 || fd >= __otter_fs_MAXOPEN)
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
