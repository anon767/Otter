#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
opts
quit
";

	common_initialization(commandString);

	return;
}
