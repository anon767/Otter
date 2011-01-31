#include <unistd.h>

extern ssize_t __otter_libc_write(int fd, const void* buf, size_t num);

ssize_t write(int fd, const void* buf, size_t num) {
    return __otter_libc_write(fd, buf, num);
}
