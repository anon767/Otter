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

	sym_file_t* sed = IOSIM_addfile("cv-vars.sed", 0);
	sed->contents = "s/^\\([a-zA-Z0-9_]*_cv_[a-zA-Z0-9_]*\\)=\\(.*\\)/: \\${\\1='\\2'}/p";
	sed->stat.st_size = 60;

	sym_file_t* input = IOSIM_addfile("cv-vars.inp", 0);
	input->contents = "_cv_=emptyvar\nac_cv_prog_RANLIB=/usr/bin/ranlib\nac_cv_prog_CC=/usr/unsupported/\\ \\ /lib/_cv_/cc\nac_cv_prog_CPP=/usr/bin/cpp\nSHELL=bash\nGNU=GNU!UNIX";
	input->stat.st_size = 147;
}