#include <unistd.h>

extern pid_t __otter_libc_getppid();

pid_t getppid()  {
    return __otter_libc_getppid();
}
