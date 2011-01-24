#include <unistd.h>

extern ssize_t __otter_libc_read(int fd, void* buf, size_t num);

ssize_t read(int fd, void* buf, size_t num) {
    return __otter_libc_read(fd, buf, num);
}
