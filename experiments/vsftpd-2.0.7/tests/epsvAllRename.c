#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
epsv all
epsv
rnfr file5
rnto file6
quit
";

	common_initialization(commandString);

	return;
}
