#include <unistd.h>

extern gid_t __otter_libc_getegid();

gid_t getegid()  {
    return __otter_libc_getegid();
}
