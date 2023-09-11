#include <stdlib.h>
#include <string.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>

ssize_t readv(int fd, const struct iovec *iov, int iovcnt)
{
	if(iovcnt <= 0 || iovcnt > IOV_MAX)
	{
		errno = EINVAL;
		return(-1);
	}
	
	long long size = 0;
	for(int i = 0; i < iovcnt; i++)
	{
		size += iov[i].iov_len;
	}
	
	if(size > UINT_MAX)
	{
		errno = EINVAL;
		return(-1);
	}
	
	char* buf = malloc((int)size); /* just do this as one big read and then copy */
	
	ssize_t r = read(fd, buf, (ssize_t)size);
	
	if(r == -1)
	{
		free(buf);
		return(-1);
	}
	
	ssize_t done = 0;
	for(int i = 0; i < iovcnt; i++)
	{
		if(done + iov[i].iov_len >= r) /* this buffer can hold the rest */
		{
			memcpy(iov[i].iov_base, buf + done, r - done);
			break;
		}
		else
		{
			memcpy(iov[i].iov_base, buf + done, iov[i].iov_len);
			done += iov[i].iov_len;
		}
	}

	free(buf);
	return(r);	
}

ssize_t writev(int fd, const struct iovec *iov, int iovcnt)
{
	if(iovcnt <= 0 || iovcnt > IOV_MAX)
	{
		errno = EINVAL;
		return(-1);
	}
	
	long long size = 0;
	for(int i = 0; i < iovcnt; i++)
	{
		size += iov[i].iov_len;
	}
	
	if(size > UINT_MAX)
	{
		errno = EINVAL;
		return(-1);
	}
	
	char* buf = malloc((int)size);
	
	ssize_t done = 0;
	for(int i = 0; i < iovcnt; i++)
	{
		memcpy(buf + done, iov[i].iov_base, iov[i].iov_len);
		done += iov[i].iov_len;
	}
	
	ssize_t r = write(fd, buf, (ssize_t)size);
	
	free(buf);
	return(r);
}
