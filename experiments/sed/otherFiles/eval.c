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

	sym_file_t* sed = IOSIM_addfile("eval.sed", 0);
	sed->contents = "1d\n\n	#Try eval command\n	/cpu/!b2\n	e../sed/sed 1q eval.in2\n\n:2\np\ni---\nh\n\n	#Try eval option\n	s,.* *cpu *,../sed/sed 1q eval.in2; echo "&",e\n\n:3\np\ng\ni---\n\n	h\n	#Try eval option with print\n	s,.* *cpu.*,../sed/sed 1q eval.in2,ep\n	g\n\n\n:4\np\ni---\n\n$!d\n\ns/.*/Doing some more tests -----------------------/p\ns,.*,../sed/sed 1q eval.in2,ep\ni---\ns,.*,../sed/sed 1q eval.in2,pe\ni---\ns,.*,../sed/sed 1q eval.in2,\nh\ne\np\ng\ni---\ns/^/echo /ep\ni---\ns/^fubar$/echo wozthis/e";
	sed->stat.st_size = 453;

	sym_file_t* input = IOSIM_addfile("eval.inp", 0);
	input->contents = "17380: 2 2 5 11 79\nabcd\ncpu\n  abcd  \n  cpu  ";
	input->stat.st_size = 44;
}