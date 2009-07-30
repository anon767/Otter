#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
stat
quit
";

	common_initialization(commandString);

	return;
}
