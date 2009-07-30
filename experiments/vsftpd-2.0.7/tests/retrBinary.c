#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
type i
pasv
retr ./file6
quit
";

	common_initialization(commandString);

	return;
}
