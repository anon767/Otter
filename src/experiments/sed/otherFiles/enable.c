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

	sym_file_t* sed = IOSIM_addfile("enable.sed", 0);
	sed->contents = "s/-*enable-//;s/=.*//";
	sed->stat.st_size = 21;

	sym_file_t* input = IOSIM_addfile("enable.inp", 0);
	input->contents = "--enable-targets=sparc-sun-sunos4.1.3,srec\n--enable-x11-testing=on\n--enable-wollybears-in-minnesota=yes-id-like-that";
	input->stat.st_size = 116;
}