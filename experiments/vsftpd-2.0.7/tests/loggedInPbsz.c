#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pbsz
quit
";

	common_initialization(commandString);

	return;
}
