#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pwd
xcup
xpwd
cdup
pwd
cwd /
pwd
xcwd ~
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
