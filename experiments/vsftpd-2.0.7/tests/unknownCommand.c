#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "badcommand
quit
";

	common_initialization(commandString);

	return;
}
