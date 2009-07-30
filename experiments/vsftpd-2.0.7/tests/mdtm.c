#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mdtm
quit
";

	common_initialization(commandString);

	return;
}
