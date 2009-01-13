#include "iosim.h"
#include <stdlib.h>
#include <sys/socket.h>
#include <errno.h>

ssize_t recv(int __fd, void *__buf, size_t __n, int __flags) {
	if (__fd >= IOSIM_num_fd || IOSIM_fd[__fd] == NULL) {
		__EVALSTR("Bad file descriptor in recv",27);
		errno = EBADF; exit();
		return -1;
	}
	IO_BUF* iobuf = IOSIM_fd[__fd];
	int num_chars_read = IOSIM_read(__fd, __buf, __n);
	if ((__flags & MSG_PEEK) && num_chars_read > 0) {
		// Reset the 'cur' pointer into the file, because we were just peeking
		iobuf->cur -= num_chars_read;
	}
	return num_chars_read;
}

//	static int recvCallCount = 0;
//	static char* recvStringList[] =
//		{
//			"USER\n",
//			"QUIT\n",
//			""
//		};
//
//	int oldCount = recvCallCount;
//	if (!(__flags & MSG_PEEK)) {
//		recvCallCount++;
//	}
//	strcpy(__buf,recvStringList[oldCount]);
//	return strlen(recvStringList[oldCount]);
//}
