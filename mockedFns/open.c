#include "iosim.h"
#include <fcntl.h>
#include <stdarg.h>

#define OPEN_DEFAULT_BUF_SIZE 1024

int open(const char *pathname, int flags, ...) {
 if (flags & O_CREAT) {
	 /* The spec states that a third argument of type mode_t 'must be
			specified when O_CREAT is in the flags, and is ignored
			otherwise.' */
	 va_list varargs;
	 va_start(varargs,flags);
	 mode_t mode = va_arg(varargs,mode_t);
	 va_end(varargs);
	 return IOSIM_openWithMode(pathname,flags,mode);
 }
 return IOSIM_open(pathname,flags);
}
