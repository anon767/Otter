#include <unistd.h>

extern pid_t __otter_libc_getpgrp();

pid_t getpgrp()  {
    return __otter_libc_getpgrp();
}
