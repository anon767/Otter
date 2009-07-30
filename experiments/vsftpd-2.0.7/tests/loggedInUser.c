#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
user
quit
";

	common_initialization(commandString);

	return;
}
