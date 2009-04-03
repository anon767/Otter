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

	sym_file_t* sed = IOSIM_addfile("numsub.sed", 0);
	sed->contents = "1s/foo/bar/10\n2s/foo/bar/20\n\nt\nd";
	sed->stat.st_size = 32;

	sym_file_t* input = IOSIM_addfile("numsub.inp", 0);
	input->contents = "foo foo fo oo f oo foo foo foo foo foo foo foo foo foo foo foo foo foo\nfoo foo fo oo f oo foo foo foo foo foo foo foo foo foo foo foo foo foo";
	input->stat.st_size = 141;
}