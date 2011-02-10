#include "otter/otter_builtins.h"

#include <signal.h>
#include <errno.h>

int kill(pid_t pid, int sig) {
    int success;
    __SYMBOLIC(&success);

    if (success) {
        return 0;
    } else {
        __SYMBOLIC(&errno);
        return -1;
    }
}
