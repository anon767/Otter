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

	sym_file_t* sed = IOSIM_addfile("newjis.sed", 0);
	sed->contents = "s/$?$1$,$-/M9JX6I/";
	sed->stat.st_size = 18;

	sym_file_t* input = IOSIM_addfile("newjis.inp", 0);
	input->contents = "となりのたけがきに\nたてかけたのは\nたてかけたかったから\nたてかけた。";
	input->stat.st_size = 91;
}