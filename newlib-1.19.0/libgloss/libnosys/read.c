/*
 * Stub version of read.
 */

#include "config.h"
#include <unistd.h>
#include <_ansi.h>
#include <_syslist.h>
#include <errno.h>
#undef errno
extern int errno;
#include "warning.h"

ssize_t
_DEFUN (_read, (file, ptr, len),
        int   file  _AND
        void *ptr   _AND
        size_t   len)
{
  errno = ENOSYS;
  return -1;
}

stub_warning(_read)
