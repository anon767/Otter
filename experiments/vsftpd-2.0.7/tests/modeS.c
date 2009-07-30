#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mode s
quit
";

	common_initialization(commandString);

	return;
}
