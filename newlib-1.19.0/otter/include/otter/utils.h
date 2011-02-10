#ifndef _OTTER_UTILS_H_
# define _OTTER_UTILS_H_

# include "otter/otter_builtins.h"
# include <errno.h>

# ifdef __OTTER_NO_FAILING_SYSCALL__

#  define __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__
#  define __OTTER_POSSIBILY_FAILING_SYSCALL_END__

# else

/* Model a possibly failing system call by introducing a
 * symbolic boolean variable and branch on it.
 */
#  define __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__  \
    { \
        int __otter_success; \
        __SYMBOLIC(&__otter_success); \
        if (!__otter_success) { \
            int __tmp; \
            __SYMBOLIC(&__tmp); \
            errno = __tmp; \
            return -1; \
        } else { 
            /* 
             * Actual body of the system call
             */
#  define __OTTER_POSSIBILY_FAILING_SYSCALL_END__  \
        } \
    }

# endif

#endif
