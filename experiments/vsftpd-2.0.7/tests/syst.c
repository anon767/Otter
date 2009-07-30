#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
syst
quit
";

	common_initialization(commandString);

	return;
}
