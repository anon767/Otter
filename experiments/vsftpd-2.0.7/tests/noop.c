#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
noop
quit
";

	common_initialization(commandString);

	return;
}
