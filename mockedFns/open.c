#include "iosim.h"
#include "umask.c"
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>

#define OPEN_DEFAULT_BUF_SIZE 1024

int open(const char *pathname, int flags, ...) {
	sym_file_t *sym_file = IOSIM_findfile(pathname);
	if (sym_file) {
		// If the file exists, then return an error if it shouldn't
		if ((flags & O_CREAT) && (flags & O_EXCL)) {
			errno = EEXIST;
			return -1;
		}
	} else if (flags & O_CREAT) {
		// File doesn't exist and we should create it
		/* The spec states that a third argument of type mode_t 'must be
			 specified when O_CREAT is in the flags, and is ignored
			 otherwise.' */
		va_list varargs;
		va_start(varargs,flags);
		mode_t mode = va_arg(varargs,mode_t);
		sym_file = IOSIM_addfile(pathname, mode & ~usermask);
	} else {
		// File doesn't exist, and we shouldn't create it.
		errno = ENOENT;
		return -1;
	}

	// Create a new stream for the file
	sym_file_stream_t *sym_stream = malloc(sizeof(sym_file_stream_t));

	// Put the stream into the 'descriptor table'.
	int fd = IOSIM_newfd();
	IOSIM_fd[fd] = sym_stream;

	// Initialize the values for the stream
	sym_stream->sym_file = sym_file;
	sym_stream->offset = 0;
//	sym_stream->fd = fd;

	return fd;
}
