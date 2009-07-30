#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
stru f
quit
";

	common_initialization(commandString);

	return;
}
