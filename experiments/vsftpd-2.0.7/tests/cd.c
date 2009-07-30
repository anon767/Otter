#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pwd
cdup
pwd
cdup
pwd
cwd /
pwd
cwd ~
pwd
cwd ~ftp
pwd
cwd
pwd
quit
";

	common_initialization(commandString);

	return;
}
