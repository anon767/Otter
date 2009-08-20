#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user ftp
pass
eprt |2|1080::8.9.10.11:800:200C:417A|5282|
retr file5
quit
";

	common_initialization(commandString);

	iosim_ip_version = AF_INET6;

	return;
}
