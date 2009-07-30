#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
prot
quit
";

	common_initialization(commandString);

	return;
}
