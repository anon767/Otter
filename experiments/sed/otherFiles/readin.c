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

	sym_file_t* sed = IOSIM_addfile("readin.sed", 0);
	sed->contents = "/\\.$/r readin.in2";
	sed->stat.st_size = 17;

	sym_file_t* input = IOSIM_addfile("readin.inp", 0);
	input->contents = "``Democracy will not come today, this year,\n  nor ever through compromise and fear.\n  I have as much right as the other fellow has\n  to stand on my two feet and own the land.\n  I tire so of hearing people say\n  let things take their course,\n  tomorrow is another day.\n  I do not need my freedom when I'm dead.\n  I cannot live on tomorrow's bread.\n  Freedom is a strong seed\n  planted in a great need.\n  I live here, too.\n  I want freedom just as you.''\n    ``The Weary Blues'', Langston Hughes";
	input->stat.st_size = 493;
}