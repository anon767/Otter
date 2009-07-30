#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
feat
quit
";

	common_initialization(commandString);

	return;
}
