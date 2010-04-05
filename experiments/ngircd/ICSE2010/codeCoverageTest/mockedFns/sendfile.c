#include <stdlib.h>
#include <sys/sendfile.h>
#include <errno.h>
#include <unistd.h>
#include "iosim.h"

// I'm basing this off of http://linux.die.net/man/2/sendfile

ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count) {
	if (in_fd >= IOSIM_num_fd || in_fd < 0 || out_fd >= IOSIM_num_fd || out_fd < 0) {
		errno = EBADF;
		return -1;
	}
	sym_file_stream_t *inbuf = IOSIM_fd[in_fd];
	if (!inbuf) {
		errno = EBADF;
		return -1;
	}

	ssize_t numSent;
	char *tempBuf = malloc(count);

	if (offset) {
		// If offset is non-null...
		// Remember the original offset
		off_t origOffset = inbuf->offset;
		// start reading from *offset
		inbuf->offset = *offset;
		numSent = read(in_fd, tempBuf, count);
		*offset += numSent; // Set *offset to new offset
		inbuf->offset = origOffset; // and reset in_fd's offset pointer
	} else {
		/* If offset is null, read from wherever in_fd was and change its
			 offset pointer */
		numSent = read(in_fd, tempBuf, count);
	}

	if (numSent == -1) {
		free(tempBuf);
		return -1;
	}

	// Write numSent bytes, in case numSent < count
	numSent = write(out_fd,tempBuf,numSent);

	free(tempBuf);

	return numSent;
}
