#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
help
quit
";

	common_initialization(commandString);

	return;
}
