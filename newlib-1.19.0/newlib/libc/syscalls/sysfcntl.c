/* connector for fcntl */

#include <reent.h>
#include <fcntl.h>

#ifdef _HAVE_STDC

#include <stdarg.h>

int
_DEFUN (fcntl, (fd, flag, ...),
     int fd _AND
     int flag _DOTS)
{
#ifdef HAVE_FCNTL
  va_list ap;
  int ret;

  va_start (ap, flag);
  ret = _fcntl_r (_REENT, fd, flag, va_arg (ap, int));
  va_end (ap);
  return ret;
#else /* !HAVE_FCNTL */
  errno = ENOSYS;
  return -1;
#endif /* !HAVE_FCNTL */
}

#else /* ! _HAVE_STDC */

#include <errno.h>

int
_DEFUN (fcntl, (fd, flag, arg),
     int fd _AND
     int flag _AND
     int arg)
{
#ifdef HAVE_FCNTL
  return _fcntl_r (_REENT, fd, flag, arg);
#else /* !HAVE_FCNTL */
  errno = ENOSYS;
  return -1;
#endif /* !HAVE_FCNTL */
}

#endif /* ! _HAVE_STDC */
