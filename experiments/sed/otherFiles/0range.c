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

	sym_file_t* sed = IOSIM_addfile("0range.sed", 0);
	sed->contents = "0,/aaa/d";
	sed->stat.st_size = 8;

	sym_file_t* input = IOSIM_addfile("0range.inp", 0);
	input->contents = "1\n2\n3\n4\naaa\nyes";
	input->stat.st_size = 15;
}