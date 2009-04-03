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

	sym_file_t* sed = IOSIM_addfile("xbxcx3.sed", 0);
	sed->contents = "s/a*/x/3";
	sed->stat.st_size = 8;

	sym_file_t* input = IOSIM_addfile("xbxcx3.inp", 0);
	input->contents = "\nb\nbc\nbac\nbaac\nbaaac\nbaaaac";
	input->stat.st_size = 27;
}