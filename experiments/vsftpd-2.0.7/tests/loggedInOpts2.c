#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
opts utf8 on
quit
";

	common_initialization(commandString);

	return;
}
