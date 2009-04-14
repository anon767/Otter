#include "iosim.h"
#include <stdio.h>

int rename(const char *old, const char *new) {
	return IOSIM_rename(old,new);
}
