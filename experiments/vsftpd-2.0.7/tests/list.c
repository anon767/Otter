#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

void symtest_initialize() {
	// Make the string of commands on fd 0
	char commandString[] = "user anonymous
pass
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

	// Make stderr
	IOSIM_fd[2] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[2]->offset = 0;
	IOSIM_fd[2]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[2]->sym_file->contents = NULL;
	IOSIM_fd[2]->sym_file->stat.st_size = 0;

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// The symbolic executor can't currently handle multiple processes
	tunable_one_process_model = 1;

#include "symbolic_values"

	return;
}
