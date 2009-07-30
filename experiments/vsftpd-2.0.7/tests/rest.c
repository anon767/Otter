#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
rest
quit
";

	common_initialization(commandString);

	return;
}
