#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
epsv all
pasv
quit
";

	common_initialization(commandString);

	return;
}
