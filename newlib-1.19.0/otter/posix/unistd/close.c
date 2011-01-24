#include <unistd.h>

extern int __otter_libc_close(int fd);

int close(int fildes) {
    return __otter_libc_close(fildes);
}
