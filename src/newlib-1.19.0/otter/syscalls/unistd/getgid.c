#include <unistd.h>

extern gid_t __otter_libc_getgid();

gid_t getgid()  {
    return __otter_libc_getgid();
}
