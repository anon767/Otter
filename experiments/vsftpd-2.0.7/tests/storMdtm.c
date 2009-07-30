#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
pasv
stor aFile
mdtm aFile
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
