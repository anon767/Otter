#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
mkd myDir
quit
";

	common_initialization(commandString);

	return;
}
