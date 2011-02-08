#include <unistd.h>
#include <fcntl.h>

int dup(int fd)
{
	return fcntl(fd, F_DUPFD, 0);
}
