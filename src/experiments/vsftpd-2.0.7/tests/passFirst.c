#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "pass
quit
";

	common_initialization(commandString);

	return;
}
