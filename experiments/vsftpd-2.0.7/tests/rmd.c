#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
rmd dir
xrmd dir
quit
";

	common_initialization(commandString);

	return;
}
