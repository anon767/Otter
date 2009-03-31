/*
 * fopen.c
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "iosim.h"

/* This depends on O_RDONLY == 0, O_WRONLY == 1, O_RDWR == 2 */

FILE *fopen(const char *file, const char *mode)
{
	int flags = O_RDONLY;
	int plus = 0;

	while (*mode) {
		switch (*mode++) {
		case 'r':
			flags = O_RDONLY;
			break;
		case 'w':
			flags = O_WRONLY | O_CREAT | O_TRUNC;
			break;
		case 'a':
			flags = O_WRONLY | O_CREAT | O_APPEND;
			break;
		case '+':
			plus = 1;
			break;
		}
	}

	if (plus) {
		flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
	}

	// IOSIM
	int fd = IOSIM_newfd();
	IO_BUF* buf = IOSIM_findfile(file);
	if(buf==NULL) return NULL;
	IOSIM_attach(fd,buf);
	return fd;
}
