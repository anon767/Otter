#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
allo
quit
";

	common_initialization(commandString);

	return;
}
