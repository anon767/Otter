#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "opts utf8 on
quit
";

	common_initialization(commandString);

	return;
}
