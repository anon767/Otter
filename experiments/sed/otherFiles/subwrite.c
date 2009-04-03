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

	sym_file_t* sed = IOSIM_addfile("subwrite.sed", 0);
	sed->contents = "s/you/YoU/w subwrite.wout";
	sed->stat.st_size = 25;

	sym_file_t* input = IOSIM_addfile("subwrite.inp", 0);
	input->contents = "Not some church, and not the state,\nNot some dark capricious fate.\nWho you are, and when you lose,\nComes only from the things you choose.";
	input->stat.st_size = 137;
}