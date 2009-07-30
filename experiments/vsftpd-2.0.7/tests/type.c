#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
type
quit
";

	common_initialization(commandString);

	return;
}
