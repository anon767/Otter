/* Stub functions for non-MultiOtter */
#include "otter/multiotter_builtins.h"

#include <stdlib.h> 
#include <unistd.h> // For size_t and pid_t

#define __otter_PID 1
#define __otter_PPID 2

void *__otter_multi_gmalloc(size_t size) {
    return malloc(size);
}

void __otter_multi_gfree(void *p) {
    free(p);
}

pid_t __otter_multi_fork(void) {
    // Caution: this is fake.
    return __otter_PID;
}

pid_t __otter_multi_get_pid(void) {
    return __otter_PID;
}

// tac calls getppid just for some random seed, 
// therefore it's possible for some non-concurrent programs to invoke concurrent functions.
pid_t __otter_multi_get_parent_pid(pid_t pid) {
    return  __otter_PPID;
}

// Let these be undefined
// void __otter_multi_set_parent_pid(pid_t pid);

// Defined in stdlib/calloc.c and realloc.c
// void *__otter_multi_gcalloc(size_t count, size_t size); 
// void *__otter_multi_grealloc(void *p, size_t size);

