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

	sym_file_t* sed = IOSIM_addfile("head.sed", 0);
	sed->contents = "3q";
	sed->stat.st_size = 2;

	sym_file_t* input = IOSIM_addfile("head.inp", 0);
	input->contents = "   "...by imposing a tiny bit of order in a communication you are\n   translating, you are carving out a little bit of order in the\n   universe.  You will never succeed.  Everything will fail and come\n   to an end finally.  But you have a chance to carve a little bit\n   of order and maybe even beauty out of the raw materials that\n   surround you everywhere, and I think there is no greater meaning\n   in life."\n\n             Donald L. Philippi, Oct 1930 - Jan 1993";
	input->stat.st_size = 465;
}