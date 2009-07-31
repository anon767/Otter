#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pasv
list -l
quit
";

	common_initialization(commandString);

	return;
}
