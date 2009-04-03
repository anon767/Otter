#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

	sym_file_t* sed = IOSIM_addfile("dollar.sed", 0);
	sed->contents = "$s/^/space /";
	sed->stat.st_size = 12;

	sym_file_t* input = IOSIM_addfile("dollar.inp", 0);
	input->contents = "I can't quite remember where I heard it,\nbut I can't seem to get out of my head\nthe phrase\nthe final frontier";
	input->stat.st_size = 109;
}