/* This is a stub that does almost nothing 
 *
 * TODO: we may want to make _execve an Otter builtin function that loads in 
 * the (pre-merged) file specified by path.
 */
#include "otter/utils.h"

#include <unistd.h>

int _execve(const char *path, char * const argv[], char * const envp[]) {

    __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__

    exit(0);

    __OTTER_POSSIBILY_FAILING_SYSCALL_END__
}
