#include "iosim.h"
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>

#define OPEN_DEFAULT_BUF_SIZE 1024

int open(char const   *__file , int __oflag  , ...) {
	IO_BUF* buf = IOSIM_findfile(__file);
	if (buf == NULL) {
		if (__oflag & O_CREAT) { // File doesn't exist; create it
			buf = IOSIM_newbuf(OPEN_DEFAULT_BUF_SIZE,malloc(OPEN_DEFAULT_BUF_SIZE));
			IOSIM_addfile(__file, buf);
		} else { // File doesn't exist and we shouldn't create it
			errno = ENOENT;
			return -1;
		}
	}
	int fd = IOSIM_newfd();
	IOSIM_attach(fd,buf);
	return fd;
}
