#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mdtm dir/file3
quit
";

	common_initialization(commandString);

	return;
}
