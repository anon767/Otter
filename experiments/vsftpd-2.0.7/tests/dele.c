#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
epsv
dele file1
dele 123/456
quit
";

	common_initialization(commandString);

	return;
}
