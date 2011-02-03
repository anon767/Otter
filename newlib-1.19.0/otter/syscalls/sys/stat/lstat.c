#include "otter/otter_fs.h"
#include <sys/stat.h>

int lstat(const char* name, struct stat* s)
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
