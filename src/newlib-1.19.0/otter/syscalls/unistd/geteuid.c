#include <unistd.h>

extern uid_t __otter_libc_geteuid();

uid_t geteuid()  {
    return __otter_libc_geteuid();
}
