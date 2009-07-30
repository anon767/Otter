#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
mdtm dir/file3
quit
";

	common_initialization(commandString);

	return;
}
