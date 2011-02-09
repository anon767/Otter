// STUB
#include "otter/otter_builtins.h"

#include <sys/resource.h>

int getpriority(int which, id_t who) {
    int prio;
    __SYMBOLIC(&prio);
    __ASSUME(prio<=PRIO_MAX);
    __ASSUME(prio>=PRIO_MIN);
    return prio;
}
