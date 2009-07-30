#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
abor
quit
";

	common_initialization(commandString);

	return;
}
