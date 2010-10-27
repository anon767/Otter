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
#define ENOTCONN 21
#define EAFNOSUPPORT 22
#define EXIO 23
#define EOVERFLOW 24
#define ENODEV 25
#define ENOTSOCK 26
#define EFAULT 27
#define EADDRINUSE 28
#define EISCONN 29
#define EOPNOTSUPP 30
#define EDESTADDRREQ 31
#define EADDRNOTAVAIL 32
#define ECONNREFUSED 33

int errno = 0;

#endif
