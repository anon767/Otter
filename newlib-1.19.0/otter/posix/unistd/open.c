#include <unistd.h>
#include <stdarg.h>

extern int __otter_libc_vopen(const char* path, int oflag, va_list varargs);

int open(const char* path, int oflag, ...) {
    va_list varargs;
    va_start(varargs, oflag);
    int result = __otter_libc_vopen(path, oflag, varargs);
    va_end(varargs);
    return result;
}
