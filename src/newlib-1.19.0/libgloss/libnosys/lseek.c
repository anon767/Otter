/*
 * Stub version of lseek.
 */

#include "config.h"
#include <sys/types.h>
#include <_ansi.h>
#include <_syslist.h>
#include <errno.h>
#undef errno
extern int errno;
#include "warning.h"

off_t
_DEFUN (_lseek, (file, ptr, dir),
        int   file  _AND
        off_t   ptr   _AND
        int   dir)
{
  errno = ENOSYS;
  return -1;
}

stub_warning(_lseek)
