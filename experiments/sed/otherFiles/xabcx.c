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

	sym_file_t* sed = IOSIM_addfile("xabcx.sed", 0);
	sed->contents = "\\xfeetxs/blue/too/";
	sed->stat.st_size = 18;

	sym_file_t* input = IOSIM_addfile("xabcx.inp", 0);
	input->contents = "roses are red\nviolets are blue\nmy feet are cold\nyour feet are blue";
	input->stat.st_size = 66;
}