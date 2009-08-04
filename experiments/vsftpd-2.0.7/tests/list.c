#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
p@sw
list
quit
";

	common_initialization(commandString);

	return;
}
