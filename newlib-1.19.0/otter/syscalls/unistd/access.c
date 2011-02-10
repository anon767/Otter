#include "otter/otter_fs.h"

#include <unistd.h>
#include <errno.h>

/* TODO:
 * Look at some of the functions that access files or directories.  
 * There is a helper function find_filename_and_dnode() (this should probably be prefixed with __otter_fs_) 
 * that finds the dnode of the parent directory so that you can use the 
 * __otter_fs_find_{inode,dnode}_in_dir() instead of traversing the path again.
 */
int access(const char *path, int amode) {
    int filep = 0;

    /* See if path is a file */
    struct __otter_fs_inode *inode = __otter_fs_find_inode(path);
    if (inode != NULL) 
        filep = inode->permissions;
    else if (errno == ENOENT) {
        /* See if path is a directory */
        struct __otter_fs_dnode *dnode = __otter_fs_find_dnode(path);
        if (dnode != NULL)
            filep = dnode->permissions;
        else return -1;
    } else 
        return -1;

    if (__otter_fs_can_permission(filep, amode))
        return 0;
    else {
        errno = EACCES;
        return -1;
    }
}
