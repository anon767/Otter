/*
 * Stub version of open.
 */

#include "config.h"
#include <_ansi.h>
#include <_syslist.h>
#include <errno.h>
#undef errno
extern int errno;
#include "warning.h"

int
_DEFUN (_open, (file, flags, ...),
        char *file  _AND
        int   flags _AND
        ...)
{
  errno = ENOSYS;
  return -1;
}

stub_warning(_open)
