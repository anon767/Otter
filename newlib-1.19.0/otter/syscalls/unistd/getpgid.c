#include <unistd.h>

extern pid_t __otter_libc_getpgid(pid_t pid);

pid_t getpgid(pid_t pid)  {
    return __otter_libc_getpgid(pid);
}
