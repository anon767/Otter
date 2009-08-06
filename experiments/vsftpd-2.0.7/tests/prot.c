#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "prot
quit
";

	common_initialization(commandString);

	return;
}
