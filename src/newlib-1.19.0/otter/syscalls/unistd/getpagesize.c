#include <unistd.h>

int getpagesize()
{
	return((int)sysconf(_SC_PAGE_SIZE));
}
