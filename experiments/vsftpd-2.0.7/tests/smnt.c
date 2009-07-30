#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
smnt
quit
";

	common_initialization(commandString);

	return;
}
