#ifndef _OTTER_UTILS_H_
# define _OTTER_UTILS_H_

# include "otter/otter_builtins.h"
# include <errno.h>

/* Similar to strchr/strrchr, but these functions returnt the integer indices of
 * the character (or -1 if not found), instead of char pointers.
 * Use these to avoid pointer minus arithmetic.
 */
int strchr_i (const char *s, int c);
int strrchr_i (const char *s, int c);

/* Allocate a char array of length (len+1), 
 * with all characters symbolic except the last one which is \0. */
char* symbolic_string(int len);

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
