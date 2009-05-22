#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

void symtest_initialize() {
	// Make the string of commands on fd 0
	static char commandString[] = "user ftp
pass
pasv
retr aFile
quit
";
	IOSIM_fd[4] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[4]->offset = 0;
	IOSIM_fd[4]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[4]->sym_file->contents = strdup(commandString);
	IOSIM_fd[4]->sym_file->stat.st_size = sizeof(commandString);
	IOSIM_fd[4]->sym_file->stat.st_mode = S_IFSOCK;

	// fd 3 is the first listening socket
	// fd 4 is the file being created (It is made by open().)
	// fd 5 is the socket from which we get the data for the file
	static char fileText[] = "something
 a bunch of text
			and some more text!!!";
	IOSIM_fd[7] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[7]->offset = 0;
	IOSIM_fd[7]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[7]->sym_file->contents = fileText;
	IOSIM_fd[7]->sym_file->stat.st_size = sizeof(fileText);


	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// The symbolic executor can't currently handle multiple processes
	tunable_one_process_model = 1;

#include "symbolic_values"




	return;
}
