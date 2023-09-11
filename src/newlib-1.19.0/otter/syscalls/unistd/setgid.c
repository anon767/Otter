#include <unistd.h>

extern int __otter_libc_setgid(gid_t gid);

int setgid(gid_t gid)  {
    return __otter_libc_setgid(gid);
}
