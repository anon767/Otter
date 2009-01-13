#include <symtest_mock.h>
#include <sys/types.h>
#include <sys/stat.h>

#if __HAVE_umask__
mode_t umask(mode_t cmask){
	return 0;
}
#endif
