#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
rein
quit
";

	common_initialization(commandString);

	return;
}
