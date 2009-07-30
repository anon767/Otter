#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mode
quit
";

	common_initialization(commandString);

	return;
}
