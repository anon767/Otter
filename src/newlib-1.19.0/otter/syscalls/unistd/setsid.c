#include <unistd.h>

extern pid_t __otter_libc_setsid();

pid_t setsid()  {
    return __otter_libc_setsid();
}
