#include "otter/otter_fs.h"

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

int dup2(int fd1, int fd2)
{
	if(fd2 < 0 || fd2 >= __otter_fs_MAX_FDS)
	{
		errno = EBADF;
		return(-1);
	}

	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd1);
	if(!open_file)
	{
		errno = EBADF;
		return(-1);
	}

	if(__otter_fs_fd_table[fd1] == __otter_fs_fd_table[fd2])
		return(fd2);

	open_file = get_open_file_from_fd(fd2);
	
	if(open_file) /* is file a valid file entry? */
	{
		close(fd2);
	}

	return fcntl(fd1, F_DUPFD, fd2);
}
