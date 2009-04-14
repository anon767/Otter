#include "iosim.h"

int unlink(const char *pathname) {
	return IOSIM_unlink(pathname);
}
