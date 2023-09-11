#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pasv
list -l f?le1
quit
";

	common_initialization(commandString);

	return;
}
