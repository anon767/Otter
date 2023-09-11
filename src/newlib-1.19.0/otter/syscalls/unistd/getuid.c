#include <unistd.h>

extern uid_t __otter_libc_getuid(void);

uid_t getuid(void)  {
    return __otter_libc_getuid();
}
