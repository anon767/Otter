#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

void symtest_initialize() {
	// Make the string of commands on fd 0
	static char commandString[] = "user anonymous
pass
epsv
stor file2
epsv
stor file1
pasv
list
dele file2
epsv
list
dele file
epsv
list
dele file1
pasv
list
dele file2
pasv
list
quit
";
	IOSIM_fd[0] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[0]->offset = 0;
	IOSIM_fd[0]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[0]->sym_file->contents = strdup(commandString);
	IOSIM_fd[0]->sym_file->stat.st_size = sizeof(commandString);
	IOSIM_fd[0]->sym_file->stat.st_mode = S_IFSOCK;

	// fd 3 is the first listening socket
	// fd 4 is the descriptor for writing the file (It is made by open().)
	// fd 5 is the socket from which we get the data for the file
	static char fileText[] = "something
 a bunch of text
			and some more text!!!";
	IOSIM_fd[5] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[5]->offset = 0;
	IOSIM_fd[5]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[5]->sym_file->contents = fileText;
	IOSIM_fd[5]->sym_file->stat.st_size = sizeof(fileText);

	static char fileText2[] = "the text of a second file";
	IOSIM_fd[8] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[8]->offset = 0;
	IOSIM_fd[8]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[8]->sym_file->contents = fileText2;
	IOSIM_fd[8]->sym_file->stat.st_size = sizeof(fileText2);

	// Every third file descriptor is a socket on which we write out information
	for (int i = 11; i < 24; i += 3) {
		IOSIM_fd[i] = malloc(sizeof(sym_file_stream_t));
		IOSIM_fd[i]->offset = 0;
		IOSIM_fd[i]->sym_file = malloc(sizeof(sym_file_t));
		IOSIM_fd[i]->sym_file->contents = NULL;
		IOSIM_fd[i]->sym_file->stat.st_size = 0;
	}

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// The symbolic executor can't currently handle multiple processes
	tunable_one_process_model = 1;

#include "symbolic_values"

	return;
}
