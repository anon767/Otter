#include "iosim.h"

extern void common_initialization(const char*);

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pasv
stou aFile
quit
";

	common_initialization(commandString);

	static char fileText[] = "something
 a bunch of text
			and some more text!!!";
	// We have to put the file on fds 8 *and* 10 because one or
	// the other is correct, depending on whether logging happens.
	IOSIM_fd[8]->sym_file->contents = fileText;
	IOSIM_fd[8]->sym_file->stat.st_size = sizeof(fileText);
	
	IOSIM_fd[10]->sym_file->contents = fileText;
	IOSIM_fd[10]->sym_file->stat.st_size = sizeof(fileText);

	return;
}
