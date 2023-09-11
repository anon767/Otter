#ifndef _MULTIOTTER_BUILTINS_H
#define _MULTIOTTER_BUILTINS_H

#include <unistd.h> // For size_t and pid_t

void *__otter_multi_gmalloc(size_t size);
void __otter_multi_gfree(void *p);
pid_t __otter_multi_fork(void);

pid_t __otter_multi_get_pid(void);
void __otter_multi_set_parent_pid(pid_t pid);
pid_t __otter_multi_get_parent_pid(pid_t pid); // Why does this take a pid_t? Shouldn't it have no arguments?

// These next two are not built-ins, but this seems like a good place to declare them, nonetheless.
void *__otter_multi_gcalloc(size_t count, size_t size);
void *__otter_multi_grealloc(void *p, size_t size);

#endif
