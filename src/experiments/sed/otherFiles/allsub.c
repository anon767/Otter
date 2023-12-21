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

	sym_file_t* sed = IOSIM_addfile("allsub.sed", 0);
	sed->contents = "s/foo/bar/g";
	sed->stat.st_size = 11;

	sym_file_t* input = IOSIM_addfile("allsub.inp", 0);
	input->contents = "foo foo fo oo f oo foo foo foo foo foo foo foo foo foo foo foo foo foo";
	input->stat.st_size = 70;
}