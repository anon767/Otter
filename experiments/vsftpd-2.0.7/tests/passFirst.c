#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "pass
quit
";

	common_initialization(commandString);

	return;
}
