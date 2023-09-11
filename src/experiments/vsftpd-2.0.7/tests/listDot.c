#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user ftp
pass
epsv 1
list .
quit
";

	common_initialization(commandString);

	return;
}
