#include <unistd.h>

extern off_t __otter_libc_lseek(int fd, off_t offset, int whence);

off_t lseek(int fd, off_t offset, int whence) {
    return __otter_libc_lseek(fd, offset, whence);
}
