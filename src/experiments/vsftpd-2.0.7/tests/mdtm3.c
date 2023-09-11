#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mdtm 20090429103015 file5
quit
";

	common_initialization(commandString);

	return;
}
