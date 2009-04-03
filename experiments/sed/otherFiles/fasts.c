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

	sym_file_t* sed = IOSIM_addfile("fasts.sed", 0);
	sed->contents = "\nh\ns/a//\np\ng\ns/a//g\np\ng\ns/^a//p\ng\ns/^a//g\np\ng\ns/not present//g\np\ng\ns/^[a-z]//g\np\ng\ns/a$//\np\ng\n\ny/a/b/\nh\ns/b//\np\ng\ns/b//g\np\ng\ns/^b//p\ng\ns/^b//g\np\ng\ns/^[a-z]//g\np\ng\ns/b$//\np\ng\n\n\n";
	sed->stat.st_size = 176;

	sym_file_t* input = IOSIM_addfile("fasts.inp", 0);
	input->contents = "aaaaaaabbbbbbaaaaaaa";
	input->stat.st_size = 20;
}