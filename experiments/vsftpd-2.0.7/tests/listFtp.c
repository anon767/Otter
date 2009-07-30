#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
list /ftp
quit
";

	common_initialization(commandString);

	return;
}
