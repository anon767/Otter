#include "otter/otter_fs.h"

#include <unistd.h>

int rmdir(const char* path)
{
	char *filename;
	struct __otter_fs_dnode* dnode = find_filename_and_dnode(path, &filename);

	if(!dnode) /* can't find path */
		return (-1);

	return (__otter_fs_rmdir_in_dir(filename, dnode) - 1);
}
