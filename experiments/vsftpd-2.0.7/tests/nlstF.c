#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pasv
nlst -F
quit
";

	common_initialization(commandString);

	return;
}
