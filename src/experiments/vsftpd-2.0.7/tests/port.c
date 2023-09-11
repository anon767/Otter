#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	// These multiple calls to port are in order to hit the error
	// conditions in the function that parses the 'port' command.
	char commandString[] = "user anonymous
pass
port
port 0,0,0,0,0,0,0
port -1,5
port 500,1
port 10,20,30,40,0,0
port 10,20,30,40,50,60
quit
";

	common_initialization(commandString);
	newStream(6);

	return;
}
