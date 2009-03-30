#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
/*
  IO_BUF* stdin = IOSIM_newbuf(-1, "-wx haha test.txt");
  IOSIM_attach(0, stdin);
*/

	// Playing fast and loose with pointers here
	// Make files for stdout and stderr
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

//	IOSIM_fd[2] = malloc(sizeof(sym_file_stream_t));
//	IOSIM_fd[2]->offset = 0;
//	IOSIM_fd[2]->fd = 2;
//	IOSIM_fd[2]->sym_file = malloc(sizeof(sym_file_t));
//	stderr = IOSIM_fd[2];


//	stderrStream->fd = 2;
//	stderrStream->offset = 0;
//	stderr = stderrStream;


  sym_file_t* file_abc = IOSIM_addfile("abc.txt", 0);
  file_abc->contents = "abc";
  file_abc->stat.st_size = 3;

  sym_file_t* file_abd = IOSIM_addfile("abd.txt", 0);
  file_abd->contents = "abd";
  file_abd->stat.st_size = 3;

  sym_file_t* file_xbc = IOSIM_addfile("xbc.txt", 0);
  file_xbc->contents = "xbc";
  file_xbc->stat.st_size = 3;

  sym_file_t* file_abbc = IOSIM_addfile("abbc.txt", 0);
  file_abbc->contents = "abbc";
  file_abbc->stat.st_size = 4;

  sym_file_t* file_abbbbc = IOSIM_addfile("abbbbc.txt", 0);
  file_abbbbc->contents = "abbbbc";
  file_abbbbc->stat.st_size = 6;

  sym_file_t* file_abcc = IOSIM_addfile("abcc.txt", 0);
  file_abcc->contents = "abcc";
  file_abcc->stat.st_size = 4;

  sym_file_t* file_ace = IOSIM_addfile("ace.txt", 0);
  file_ace->contents = "ace";
  file_ace->stat.st_size = 3;



/*
  sym_file_stream_t *fileS = malloc(sizeof(sym_file_stream_t));
  fileS->sym_file = fileT;
  fileS->offset = 0;
*/

/*  IO_BUF* sed_exp = IOSIM_newbuf(-1, "1haha1\n2haha2\n3haha3");
  IOSIM_addfile("test.txt", fileS);
*/
}

