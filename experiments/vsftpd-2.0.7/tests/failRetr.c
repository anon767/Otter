#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
retr aFile
quit
";

	common_initialization(commandString);

	return;
}
