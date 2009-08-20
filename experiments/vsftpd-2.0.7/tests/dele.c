#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
epsv 2
dele file1
dele 123/456
quit
";

	common_initialization(commandString);

	return;
}
