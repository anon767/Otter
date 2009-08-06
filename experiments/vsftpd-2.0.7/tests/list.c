#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
p@sw
list
quit
";

	common_initialization(commandString);

	return;
}
