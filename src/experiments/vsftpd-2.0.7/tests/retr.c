#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user ftp
pass
port 10,20,30,40,50,60
retr file5
quit
";

	common_initialization(commandString);

	return;
}
