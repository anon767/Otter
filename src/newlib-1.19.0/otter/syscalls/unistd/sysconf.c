#include <unistd.h>
#include <errno.h>

long sysconf(int name)
{
	switch(name)
	{
		case _SC_PAGE_SIZE: /* _SC_PAGESIZE as well */
			return(4096);
		/* TODO: Impliment other system constants */
	}
	
	errno = EINVAL;
	return(-1);
}
