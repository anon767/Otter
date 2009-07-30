#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "auth
quit
";

	common_initialization(commandString);

	return;
}
