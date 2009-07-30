#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
rmd dir
quit
";

	common_initialization(commandString);

	return;
}
