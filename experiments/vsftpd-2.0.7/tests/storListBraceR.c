#include "iosim.h"

void symtest_initialize() {
	char commandString[] = "user anonymous
pass
epsv
stor myfile2
pasv
list -r
quit
";
//list -r file{2,1}
//quit
//";

	common_initialization(commandString);

	// fd 5 is the first listening socket
	// fd 6 is the descriptor for writing the file (It is made by open().)
	// fd 7 is the socket from which we get the data for the file
	char fileText[] = "something
 a bunch of text
			and some more text!!!";
	IOSIM_fd[7]->sym_file->contents = fileText;
	IOSIM_fd[7]->sym_file->stat.st_size = sizeof(fileText);

	return;
}
