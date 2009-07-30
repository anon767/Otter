#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
list /
quit
";

	common_initialization(commandString);

	return;
}
