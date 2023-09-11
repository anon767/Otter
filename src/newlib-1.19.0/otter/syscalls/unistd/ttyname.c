#include "otter/otter_fs.h"

#include <unistd.h>

char * ttyname(int fd) {
    struct __otter_fs_open_file_table_entry* entry = get_open_file_from_fd(fd);

    if (entry->type != __otter_fs_TYP_TTY) 
        return NULL;
    else
        return "/dev/tty"; // TODO: get the real name
}

