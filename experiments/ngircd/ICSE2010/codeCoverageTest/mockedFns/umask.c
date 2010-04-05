#include <sys/stat.h>
#include <sys/types.h>
static usermask;
mode_t umask(mode_t cmask) {
	mode_t oldMask = usermask;
	usermask = cmask & 0777;
	return oldMask;
}
