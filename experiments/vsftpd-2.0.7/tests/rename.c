#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
rnfr file5
rnto file6
rnto file5
rnfr file5
quit
";

	common_initialization(commandString);

	return;
}
