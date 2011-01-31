#include <sys/stat.h>

extern int __otter_libc_fstat(int fildes, struct stat* buf);

int fstat(int fildes, struct stat *buf) {
    return __otter_libc_fstat(fildes, buf);
}
