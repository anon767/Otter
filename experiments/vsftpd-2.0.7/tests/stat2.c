#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
stat dir/file2
quit
";

	common_initialization(commandString);

	return;
}
