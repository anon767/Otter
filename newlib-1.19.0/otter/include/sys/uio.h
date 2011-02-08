#ifndef _SYS_UIO_H
#define _SYS_UIO_H

#include <sys/types.h>

struct iovec
{
	void* iov_base;
	size_t iov_len;
};

#define IOV_MAX 256
#define UIO_MAXIOV IOV_MAX

ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);

#endif
