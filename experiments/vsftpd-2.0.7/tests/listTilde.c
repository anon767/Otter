#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
list ~ftp/123
quit
";

	common_initialization(commandString);

	return;
}
