#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
acct
quit
";

	common_initialization(commandString);

	return;
}
