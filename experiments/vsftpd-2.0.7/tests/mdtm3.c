#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mdtm 20090429103015 file5
quit
";

	common_initialization(commandString);

	return;
}
