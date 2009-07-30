#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
retr file5
quit
";

	common_initialization(commandString);

	return;
}
