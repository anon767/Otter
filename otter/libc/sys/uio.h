#ifndef _SYS_UIO_H
#define _SYS_UIO_H

#include<sys/types.h>

struct iovec
{
	void* iov_base;
	size_t iov_len;
};

#endif
