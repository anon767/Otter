#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

//#if __HAVE_fcntl__
int fcntl(int fildes, int cmd, ...){
	// arbitrary
	return 0;
}
//#endif
