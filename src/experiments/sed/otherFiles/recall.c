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

	sym_file_t* sed = IOSIM_addfile("recall.sed", 0);
	sed->contents = "p\ns/e/X/p\n:x\ns//Y/p\n/f/bx";
	sed->stat.st_size = 25;

	sym_file_t* input = IOSIM_addfile("recall.inp", 0);
	input->contents = "eeefff";
	input->stat.st_size = 6;
}