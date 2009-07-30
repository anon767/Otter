#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user ftp
pass
pasv
stor aFile
pasv
list /ftp
quit
";

	common_initialization(commandString);

	char fileText[] = "something
 a bunch of text
			and some more text!!!";
	IOSIM_fd[7]->sym_file->contents = fileText;
	IOSIM_fd[7]->sym_file->stat.st_size = sizeof(fileText);

	return;
}
