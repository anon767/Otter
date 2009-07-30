#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
type a
quit
";

	common_initialization(commandString);

	return;
}
