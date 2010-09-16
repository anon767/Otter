#ifndef _ERRNO_H
#define _ERRNO_H

#define EDOM 1
#define ERANGE 2

/* I/O */
#define EACCESS 3
#define ELOOP 4
#define ENAMETOOLONG 5
#define ENOTDIR 6
#define ENOENT 7
#define EPERM 8
#define EROFS 9
#define EBADF 10
#define EEXIST 11
#define EMFILE 12
#define ENFILE 13
#define EISDIR 14
#define EINVAL 15
#define EAGAIN 16
#define ESPIPE 17
#define EPIPE 18
#define ENOTEMPTY 19
#define EINTR 20

int errno = 0;

#endif
