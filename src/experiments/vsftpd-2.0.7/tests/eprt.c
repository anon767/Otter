#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	// These multiple calls to eprt are in order to hit the error
	// conditions in the function that parses the 'eprt' command.
	char commandString[] = "user anonymous
pass
eprt x
eprt
eprt |3|
eprt |2|
eprt |1|1.2.3.4
eprt |1|1.2.3.4||
eprt |1|1.2.3.4||x
eprt |1|1.2.3.4|-1|
eprt |1|1.2.3.4|65536|
eprt |1|1.2.3.4|0|
eprt |1|10.20.30.40|0|
eprt |1|10.20.30.40|10000|
quit
";

	common_initialization(commandString);

	return;
}
