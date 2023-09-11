#include "otter/utils.h"

#include <sys/resource.h>
#include <errno.h>

int setpriority(int which, id_t who, int prio) {

    __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__

    _SYS_RESOURCE_PRIORITY_TABLE[which][who].prio = prio;
    _SYS_RESOURCE_PRIORITY_TABLE[which][who].has_set = 1;
    return 0;

    __OTTER_POSSIBILY_FAILING_SYSCALL_END__
}

