#include <sys/time.h>
#include <sys/types.h>
#include <sys/select.h> 

#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO

typedef struct fd_set 
{
  unsigned int count;
  int fd[FD_SETSIZE];
} fd_set;

void FD_SET(int fd, fd_set *fdset)
{
	if (fdset->count < FD_SETSIZE)
		fdset->fd[fdset->count++] = fd;
}

void FD_CLR(int fd, fd_set *fdset)
{
	unsigned int i;

	for (i = 0; i < fdset->count ; i++) {
		if (fdset->fd[i] == fd) {
			while (i < fdset->count - 1) {
				fdset->fd[i] = fdset->fd[i + 1];
				i++;
			}
			fdset->count--;
			break;
		}
	}
}

int FD_ISSET(int fd, fd_set *fdset)
{
	unsigned int i;
	for (i = 0; i < fdset->count; i++) {
		if (fdset->fd[i] == fd) return 1;
	}

	return 0;
}

void FD_ZERO(fd_set *fdset)
{
	fdset->count = 0;
}
