#include "otter/otter_fs.h"

#include <unistd.h>
#include <errno.h>

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
