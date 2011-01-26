#include <sys/stat.h>

extern int __otter_libc_lstat(const char* name, struct stat* s);

int lstat(const char* name, struct stat* s) {
    return __otter_libc_lstat(name, s);
}
