#include "otter/otter_builtins.h"

#include <sys/resource.h>
#include <errno.h>

int getpriority(int which, id_t who) {
    int success; 
    __SYMBOLIC(&success);

    if (success) {
        if (!_SYS_RESOURCE_PRIORITY_TABLE[which][who].has_set) {
            // If it hasn't been set, we assume it's unknown, hence symbolic.
            int prio;
            __SYMBOLIC(&prio);
            __ASSUME(prio<=PRIO_MAX);
            __ASSUME(prio>=PRIO_MIN);
            _SYS_RESOURCE_PRIORITY_TABLE[which][who].prio = prio;
            _SYS_RESOURCE_PRIORITY_TABLE[which][who].has_set = 1;
        }
        return _SYS_RESOURCE_PRIORITY_TABLE[which][who].prio;
    } else {
        __SYMBOLIC(&errno);
        return -1;
    }
}
