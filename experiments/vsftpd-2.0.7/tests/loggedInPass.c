#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pass
quit
";

	common_initialization(commandString);

	return;
}
