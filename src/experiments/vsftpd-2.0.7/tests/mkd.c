#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user ftp
pass
mkd myDir
xmkd myDir2
quit
";

	common_initialization(commandString);

	return;
}
