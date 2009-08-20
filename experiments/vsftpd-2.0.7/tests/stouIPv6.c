#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
eprt |2|1080::8.9.10.11:800:200C:417A|5282|
stou aFile
quit
";

	common_initialization(commandString);

	static char fileText[] = "something
 a bunch of text
			and some more text!!!";
	IOSIM_fd[7]->sym_file->contents = fileText;
	IOSIM_fd[7]->sym_file->stat.st_size = sizeof(fileText);

	iosim_ip_version = AF_INET6;

	return;
}
