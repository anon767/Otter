#include "iosim.h"
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

FILE * fopen (const char * filename, const char * mode)
{
	int openFlags, plus = 0;
	if (!(mode && mode[0])) { // If mode is null or ""
		errno = EINVAL;
		return NULL;
	}

	if (mode[1] == '+' || (mode[1] == 'b' && mode[2] == '+')) {
		plus = 1;
	}

	switch (mode[0]) {
	case 'r':
		openFlags = plus ? O_RDWR : O_RDONLY;
		break;
	case 'w':
		openFlags = O_CREAT | O_TRUNC | (plus ? O_RDWR : O_WRONLY);
		break;
	case 'a':
		openFlags = O_CREAT | O_APPEND | (plus ? O_RDWR : O_WRONLY);
		break;
	default:
		errno = EINVAL;
		return NULL;
	}

	int fd = open(filename, openFlags, 0666); // 0666 is stated in the spec
	if (fd == -1) {
		return NULL;
	}
	return IOSIM_fd[fd];
}
