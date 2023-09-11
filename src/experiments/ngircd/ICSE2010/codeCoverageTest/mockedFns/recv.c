#include "iosim.h"
#include <stdlib.h>
#include <sys/socket.h>
#include <errno.h>

ssize_t recv(int sockfd, void *buf, size_t len, int flags) {
	if (sockfd < 0 || sockfd >= IOSIM_num_fd || IOSIM_fd[sockfd] == NULL) {
		errno = EBADF;
		return -1;
	}
	int num_chars_read = IOSIM_read(sockfd, buf, len);
	if (flags & MSG_PEEK) {
		// Reset the 'cur' pointer into the file, because we were just peeking
		IOSIM_fd[sockfd]->offset -= num_chars_read;
	}
	return num_chars_read;
}
