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

	sym_file_t* sed = IOSIM_addfile("numsub4.sed", 0);
	sed->contents = "s/^a*/b/2p";
	sed->stat.st_size = 10;

	sym_file_t* input = IOSIM_addfile("numsub4.inp", 0);
	input->contents = "z";
	input->stat.st_size = 1;
}