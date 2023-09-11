#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "opts
quit
";

	common_initialization(commandString);

	return;
}
