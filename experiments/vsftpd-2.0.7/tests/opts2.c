#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "opts utf8 on
quit
";

	common_initialization(commandString);

	return;
}
