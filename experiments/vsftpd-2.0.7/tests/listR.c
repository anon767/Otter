#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
port 10,20,30,40,50,60
list -R
quit
";

	common_initialization(commandString);

	return;
}
