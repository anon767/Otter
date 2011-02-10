#include "otter/otter_builtins.h"

#include <sys/resource.h>
#include <errno.h>

int setpriority(int which, id_t who, int prio) {
    int success; 
    __SYMBOLIC(&success);

    if (success) {
        _SYS_RESOURCE_PRIORITY_TABLE[which][who].prio = prio;
        _SYS_RESOURCE_PRIORITY_TABLE[which][who].has_set = 1;
        return 0;
    } else {
        __SYMBOLIC(&errno);
        return -1;
    }
}

