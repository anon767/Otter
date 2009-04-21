#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

void symtest_initialize() {
	// Make the string of commands on fd 0
	char commandString[] = "user ftp
pass
badcommand
quit
";
	IOSIM_fd[0] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[0]->offset = 0;
	IOSIM_fd[0]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[0]->sym_file->contents = strdup(commandString);
	IOSIM_fd[0]->sym_file->stat.st_size = sizeof(commandString);
	IOSIM_fd[0]->sym_file->stat.st_mode = S_IFSOCK;

	IOSIM_fd[2] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[2]->offset = 0;
	IOSIM_fd[2]->fd = 1;
	IOSIM_fd[2]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[2]->sym_file->contents = NULL;
	IOSIM_fd[2]->sym_file->stat.st_size = 0;
	stderr = IOSIM_fd[2];

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

#include "symbolic_values"

	return;
}
