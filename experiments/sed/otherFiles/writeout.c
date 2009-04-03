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

	sym_file_t* sed = IOSIM_addfile("writeout.sed", 0);
	sed->contents = "/^Facts ar/w writeout.wout";
	sed->stat.st_size = 26;

	sym_file_t* input = IOSIM_addfile("writeout.inp", 0);
	input->contents = "Facts are simple and facts are straight\nFacts are lazy and facts are late\nFacts all come with points of view\nFacts don't do what I want them to";
	input->stat.st_size = 143;
}