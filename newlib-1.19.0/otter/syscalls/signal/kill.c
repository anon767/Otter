#include "otter/utils.h"

#include <signal.h>

int kill(pid_t pid, int sig) {

    __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__

    return 0;

    __OTTER_POSSIBILY_FAILING_SYSCALL_END__
}
