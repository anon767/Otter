#include <unistd.h>

extern int __otter_libc_setuid(uid_t uid);

int seteuid(uid_t uid)  {
    return __otter_libc_setuid(uid);
}
